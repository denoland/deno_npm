// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::HashMap;

use deno_semver::StackString;
use deno_semver::Version;
use deno_semver::VersionReq;
use deno_semver::WILDCARD_VERSION_REQ;
use deno_semver::package::PackageName;
use deno_semver::package::PackageNv;
use thiserror::Error;

use crate::registry::NpmPackageInfo;
use crate::registry::NpmPackageVersionInfo;

/// Error that occurs when the version is not found in the package information.
#[derive(Debug, Error, Clone, deno_error::JsError)]
#[class(type)]
#[error("Could not find version '{}' for npm package '{}'.", .0.version, .0.name)]
pub struct NpmPackageVersionNotFound(pub PackageNv);

#[derive(Debug, Error, Clone, deno_error::JsError)]
pub enum NpmPackageVersionResolutionError {
  #[class(type)]
  #[error(
    "Could not find dist-tag '{dist_tag}' for npm package '{package_name}'."
  )]
  DistTagNotFound {
    dist_tag: String,
    package_name: StackString,
  },
  #[class(type)]
  #[error(
    "Could not find version '{version}' referenced in dist-tag '{dist_tag}' for npm package '{package_name}'."
  )]
  DistTagVersionNotFound {
    package_name: StackString,
    dist_tag: String,
    version: String,
  },
  #[class(inherit)]
  #[error(transparent)]
  VersionNotFound(#[from] NpmPackageVersionNotFound),
  #[class(type)]
  #[error(
    "Could not find npm package '{package_name}' matching '{version_req}'."
  )]
  VersionReqNotMatched {
    package_name: StackString,
    version_req: VersionReq,
  },
}

#[derive(Debug, Clone)]
pub struct NpmVersionResolver<'link> {
  pub types_node_version_req: Option<VersionReq>,
  pub link_packages: &'link HashMap<PackageName, Vec<NpmPackageVersionInfo>>,
}

impl NpmVersionResolver<'_> {
  pub fn resolve_best_package_version_info<'a, 'version>(
    &'a self,
    version_req: &VersionReq,
    package_info: &'a NpmPackageInfo,
    existing_versions: impl Iterator<Item = &'version Version>,
  ) -> Result<&'a NpmPackageVersionInfo, NpmPackageVersionResolutionError> {
    // always attempt to resolve from the linked packages first
    if let Some(version_infos) = self.link_packages.get(&package_info.name) {
      let mut best_version: Option<&'a NpmPackageVersionInfo> = None;
      for version_info in version_infos {
        let version = &version_info.version;
        if self.version_req_satisfies(version_req, version, package_info)? {
          let is_greater =
            best_version.map(|c| *version > c.version).unwrap_or(true);
          if is_greater {
            best_version = Some(version_info);
          }
        }
      }
      if let Some(top_version) = best_version {
        return Ok(top_version);
      }
    }

    if let Some(version) = self.resolve_best_from_existing_versions(
      version_req,
      package_info,
      existing_versions,
    )? {
      match package_info.versions.get(version) {
        Some(version_info) => Ok(version_info),
        None => Err(NpmPackageVersionResolutionError::VersionNotFound(
          NpmPackageVersionNotFound(PackageNv {
            name: package_info.name.clone(),
            version: version.clone(),
          }),
        )),
      }
    } else {
      // get the information
      self.get_resolved_package_version_and_info(version_req, package_info)
    }
  }

  fn get_resolved_package_version_and_info<'a>(
    &self,
    version_req: &VersionReq,
    info: &'a NpmPackageInfo,
  ) -> Result<&'a NpmPackageVersionInfo, NpmPackageVersionResolutionError> {
    if let Some(tag) = version_req.tag() {
      self.tag_to_version_info(info, tag)
      // When the version is *, if there is a latest tag, use it directly.
      // No need to care about @types/node here, because it'll be handled specially below.
    } else if info.dist_tags.contains_key("latest")
      && info.name != "@types/node"
      && (*version_req == *WILDCARD_VERSION_REQ
        // When the latest tag satisfies the version requirement, use it directly.
        // https://github.com/npm/npm-pick-manifest/blob/67508da8e21f7317e3159765006da0d6a0a61f84/lib/index.js#L125
        || info
          .dist_tags
          .get("latest")
          .and_then(|version| {
            self.version_req_satisfies(version_req, version, info).ok()
          })
          .unwrap_or_default())
    {
      self.tag_to_version_info(info, "latest")
    } else {
      let mut maybe_best_version: Option<&'a NpmPackageVersionInfo> = None;
      for version_info in info.versions.values() {
        let version = &version_info.version;
        if self.version_req_satisfies(version_req, version, info)? {
          let is_best_version = maybe_best_version
            .as_ref()
            .map(|best_version| best_version.version.cmp(version).is_lt())
            .unwrap_or(true);
          if is_best_version {
            maybe_best_version = Some(version_info);
          }
        }
      }

      match maybe_best_version {
        Some(v) => Ok(v),
        // Although it seems like we could make this smart by fetching the latest
        // information for this package here, we really need a full restart. There
        // could be very interesting bugs that occur if this package's version was
        // resolved by something previous using the old information, then now being
        // smart here causes a new fetch of the package information, meaning this
        // time the previous resolution of this package's version resolved to an older
        // version, but next time to a different version because it has new information.
        None => Err(NpmPackageVersionResolutionError::VersionReqNotMatched {
          package_name: info.name.clone(),
          version_req: version_req.clone(),
        }),
      }
    }
  }

  pub fn version_req_satisfies(
    &self,
    version_req: &VersionReq,
    version: &Version,
    package_info: &NpmPackageInfo,
  ) -> Result<bool, NpmPackageVersionResolutionError> {
    match version_req.tag() {
      Some(tag) => {
        let version_info = self.tag_to_version_info(package_info, tag)?;
        Ok(version_info.version == *version)
      }
      None => {
        // For when someone just specifies @types/node, we want to pull in a
        // "known good" version of @types/node that works well with Deno and
        // not necessarily the latest version. For example, we might only be
        // compatible with Node vX, but then Node vY is published so we wouldn't
        // want to pull that in.
        // Note: If the user doesn't want this behavior, then they can specify an
        // explicit version.
        if package_info.name == "@types/node"
          && *version_req == *WILDCARD_VERSION_REQ
          && let Some(version_req) = &self.types_node_version_req
        {
          return Ok(version_req.matches(version));
        }

        Ok(version_req.matches(version))
      }
    }
  }

  fn resolve_best_from_existing_versions<'a>(
    &self,
    version_req: &VersionReq,
    package_info: &NpmPackageInfo,
    existing_versions: impl Iterator<Item = &'a Version>,
  ) -> Result<Option<&'a Version>, NpmPackageVersionResolutionError> {
    let mut maybe_best_version: Option<&Version> = None;
    for version in existing_versions {
      if self.version_req_satisfies(version_req, version, package_info)? {
        let is_best_version = maybe_best_version
          .as_ref()
          .map(|best_version| (*best_version).cmp(version).is_lt())
          .unwrap_or(true);
        if is_best_version {
          maybe_best_version = Some(version);
        }
      }
    }
    Ok(maybe_best_version)
  }

  fn tag_to_version_info<'a>(
    &self,
    info: &'a NpmPackageInfo,
    tag: &str,
  ) -> Result<&'a NpmPackageVersionInfo, NpmPackageVersionResolutionError> {
    if let Some(version) = info.dist_tags.get(tag) {
      match info.versions.get(version) {
        Some(info) => Ok(info),
        None => Err(NpmPackageVersionResolutionError::DistTagVersionNotFound {
          package_name: info.name.clone(),
          dist_tag: tag.to_string(),
          version: version.to_string(),
        }),
      }
    } else {
      Err(NpmPackageVersionResolutionError::DistTagNotFound {
        package_name: info.name.clone(),
        dist_tag: tag.to_string(),
      })
    }
  }
}

#[cfg(test)]
mod test {
  use std::collections::HashMap;

  use deno_semver::package::PackageReq;

  use super::*;

  #[test]
  fn test_get_resolved_package_version_and_info() {
    // dist tag where version doesn't exist
    let package_req = PackageReq::from_str("test@latest").unwrap();
    let package_info = NpmPackageInfo {
      name: "test".into(),
      versions: HashMap::new(),
      dist_tags: HashMap::from([(
        "latest".into(),
        Version::parse_from_npm("1.0.0-alpha").unwrap(),
      )]),
    };
    let resolver = NpmVersionResolver {
      types_node_version_req: None,
      link_packages: &Default::default(),
    };
    let result = resolver.get_resolved_package_version_and_info(
      &package_req.version_req,
      &package_info,
    );
    assert_eq!(
      result.err().unwrap().to_string(),
      "Could not find version '1.0.0-alpha' referenced in dist-tag 'latest' for npm package 'test'."
    );

    // dist tag where version is a pre-release
    let package_req = PackageReq::from_str("test@latest").unwrap();
    let package_info = NpmPackageInfo {
      name: "test".into(),
      versions: HashMap::from([
        (
          Version::parse_from_npm("0.1.0").unwrap(),
          NpmPackageVersionInfo::default(),
        ),
        (
          Version::parse_from_npm("1.0.0-alpha").unwrap(),
          NpmPackageVersionInfo {
            version: Version::parse_from_npm("1.0.0-alpha").unwrap(),
            ..Default::default()
          },
        ),
      ]),
      dist_tags: HashMap::from([(
        "latest".into(),
        Version::parse_from_npm("1.0.0-alpha").unwrap(),
      )]),
    };
    let result = resolver.get_resolved_package_version_and_info(
      &package_req.version_req,
      &package_info,
    );
    assert_eq!(result.unwrap().version.to_string(), "1.0.0-alpha");
  }

  #[test]
  fn test_types_node_version() {
    // this will use the 1.0.0 version because that's what was specified
    // for the "types_node_version_req" even though the latest is 1.1.0
    let package_req = PackageReq::from_str("@types/node").unwrap();
    let package_info = NpmPackageInfo {
      name: "@types/node".into(),
      versions: HashMap::from([
        (
          Version::parse_from_npm("1.0.0").unwrap(),
          NpmPackageVersionInfo {
            version: Version::parse_from_npm("1.0.0").unwrap(),
            ..Default::default()
          },
        ),
        (
          Version::parse_from_npm("1.1.0").unwrap(),
          NpmPackageVersionInfo {
            version: Version::parse_from_npm("1.1.0").unwrap(),
            ..Default::default()
          },
        ),
      ]),
      dist_tags: HashMap::from([(
        "latest".into(),
        Version::parse_from_npm("1.1.0").unwrap(),
      )]),
    };
    let resolver = NpmVersionResolver {
      types_node_version_req: Some(
        VersionReq::parse_from_npm("1.0.0").unwrap(),
      ),
      link_packages: &Default::default(),
    };
    let result = resolver.get_resolved_package_version_and_info(
      &package_req.version_req,
      &package_info,
    );
    assert_eq!(result.unwrap().version.to_string(), "1.0.0");
  }

  #[test]
  fn test_wildcard_version_req() {
    let package_req = PackageReq::from_str("some-pkg").unwrap();
    let package_info = NpmPackageInfo {
      name: "some-pkg".into(),
      versions: HashMap::from([
        (
          Version::parse_from_npm("1.0.0-rc.1").unwrap(),
          NpmPackageVersionInfo {
            version: Version::parse_from_npm("1.0.0-rc.1").unwrap(),
            ..Default::default()
          },
        ),
        (
          Version::parse_from_npm("2.0.0").unwrap(),
          NpmPackageVersionInfo {
            version: Version::parse_from_npm("2.0.0").unwrap(),
            ..Default::default()
          },
        ),
      ]),
      dist_tags: HashMap::from([(
        "latest".into(),
        Version::parse_from_npm("1.0.0-rc.1").unwrap(),
      )]),
    };
    let resolver = NpmVersionResolver {
      types_node_version_req: None,
      link_packages: &Default::default(),
    };
    let result = resolver.get_resolved_package_version_and_info(
      &package_req.version_req,
      &package_info,
    );
    assert_eq!(result.unwrap().version.to_string(), "1.0.0-rc.1");
  }

  #[test]
  fn test_latest_tag_version_req() {
    let package_info = NpmPackageInfo {
      name: "some-pkg".into(),
      versions: HashMap::from([
        (
          Version::parse_from_npm("0.1.0-alpha.1").unwrap(),
          NpmPackageVersionInfo {
            version: Version::parse_from_npm("0.1.0-alpha.1").unwrap(),
            ..Default::default()
          },
        ),
        (
          Version::parse_from_npm("0.1.0-alpha.2").unwrap(),
          NpmPackageVersionInfo {
            version: Version::parse_from_npm("0.1.0-alpha.2").unwrap(),
            ..Default::default()
          },
        ),
        (
          Version::parse_from_npm("0.1.0-beta.1").unwrap(),
          NpmPackageVersionInfo {
            version: Version::parse_from_npm("0.1.0-beta.1").unwrap(),
            ..Default::default()
          },
        ),
        (
          Version::parse_from_npm("0.1.0-beta.2").unwrap(),
          NpmPackageVersionInfo {
            version: Version::parse_from_npm("0.1.0-beta.2").unwrap(),
            ..Default::default()
          },
        ),
      ]),
      dist_tags: HashMap::from([
        (
          "latest".into(),
          Version::parse_from_npm("0.1.0-alpha.2").unwrap(),
        ),
        (
          "dev".into(),
          Version::parse_from_npm("0.1.0-beta.2").unwrap(),
        ),
      ]),
    };
    let resolver = NpmVersionResolver {
      types_node_version_req: None,
      link_packages: &Default::default(),
    };

    // check for when matches dist tag
    let package_req = PackageReq::from_str("some-pkg@^0.1.0-alpha.2").unwrap();
    let result = resolver.get_resolved_package_version_and_info(
      &package_req.version_req,
      &package_info,
    );
    assert_eq!(
      result.unwrap().version.to_string(),
      "0.1.0-alpha.2" // not "0.1.0-beta.2"
    );

    // check for when not matches dist tag
    let package_req = PackageReq::from_str("some-pkg@^0.1.0-beta.2").unwrap();
    let result = resolver.get_resolved_package_version_and_info(
      &package_req.version_req,
      &package_info,
    );
    assert_eq!(result.unwrap().version.to_string(), "0.1.0-beta.2");
  }
}
