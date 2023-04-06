// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use deno_semver::npm::NpmPackageNv;
use deno_semver::Version;
use deno_semver::VersionReq;
use once_cell::sync::Lazy;
use thiserror::Error;

use crate::registry::NpmPackageInfo;
use crate::registry::NpmPackageVersionInfo;

pub static LATEST_VERSION_REQ: Lazy<VersionReq> =
  Lazy::new(|| VersionReq::parse_from_specifier("latest").unwrap());

/// Error that occurs when the version is not found in the package information.
#[derive(Debug, Error, Clone)]
#[error("Could not find version '{}' for npm package '{}'.", .0.version, .0.name)]
pub struct NpmPackageVersionNotFound(pub NpmPackageNv);

#[derive(Debug, Error, Clone)]
pub enum NpmPackageVersionResolutionError {
  #[error(
    "Could not find dist-tag '{dist_tag}' for npm package '{package_name}'."
  )]
  DistTagNotFound {
    dist_tag: String,
    package_name: String,
  },
  #[error(
    "Could not find version '{version}' referenced in dist-tag '{dist_tag}' for npm package '{package_name}'."
  )]
  DistTagVersionNotFound {
    package_name: String,
    dist_tag: String,
    version: String,
  },
  #[error(transparent)]
  VersionNotFound(#[from] NpmPackageVersionNotFound),
  #[error(
    "Could not find npm package '{package_name}' matching '{version_req}'."
  )]
  VersionReqNotMatched {
    package_name: String,
    version_req: VersionReq,
  },
}

pub fn resolve_best_package_version_info<'info, 'version>(
  version_req: &VersionReq,
  package_info: &'info NpmPackageInfo,
  existing_versions: impl Iterator<Item = &'version Version>,
) -> Result<&'info NpmPackageVersionInfo, NpmPackageVersionResolutionError> {
  if let Some(version) = resolve_best_from_existing_versions(
    version_req,
    package_info,
    existing_versions,
  )? {
    match package_info.versions.get(version) {
      Some(version_info) => Ok(version_info),
      None => Err(NpmPackageVersionResolutionError::VersionNotFound(
        NpmPackageVersionNotFound(NpmPackageNv {
          name: package_info.name.clone(),
          version: version.clone(),
        }),
      )),
    }
  } else {
    // get the information
    get_resolved_package_version_and_info(version_req, package_info)
  }
}

fn get_resolved_package_version_and_info<'a>(
  version_req: &VersionReq,
  info: &'a NpmPackageInfo,
) -> Result<&'a NpmPackageVersionInfo, NpmPackageVersionResolutionError> {
  if let Some(tag) = version_req.tag() {
    tag_to_version_info(info, tag)
  } else {
    let mut maybe_best_version: Option<&'a NpmPackageVersionInfo> = None;
    for version_info in info.versions.values() {
      let version = &version_info.version;
      if version_req.matches(version) {
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
      // If the package isn't found, it likely means that the user needs to use
      // `--reload` in the CLI to get the latest npm package information. Although
      // it seems like we could make this smart by fetching the latest information for
      // this package here, we really need a full restart. There could be very
      // interesting bugs that occur if this package's version was resolved by
      // something previous using the old information, then now being smart here
      // causes a new fetch of the package information, meaning this time the
      // previous resolution of this package's version resolved to an older
      // version, but next time to a different version because it has new information.
      None => Err(NpmPackageVersionResolutionError::VersionReqNotMatched {
        package_name: info.name.clone(),
        version_req: version_req.clone(),
      }),
    }
  }
}

pub fn version_req_satisfies(
  version_req: &VersionReq,
  version: &Version,
  package_info: &NpmPackageInfo,
) -> Result<bool, NpmPackageVersionResolutionError> {
  match version_req.tag() {
    Some(tag) => {
      let version_info = tag_to_version_info(package_info, tag)?;
      Ok(version_info.version == *version)
    }
    None => Ok(version_req.matches(version)),
  }
}

fn resolve_best_from_existing_versions<'a>(
  version_req: &VersionReq,
  package_info: &NpmPackageInfo,
  existing_versions: impl Iterator<Item = &'a Version>,
) -> Result<Option<&'a Version>, NpmPackageVersionResolutionError> {
  let mut maybe_best_version: Option<&Version> = None;
  for version in existing_versions {
    if version_req_satisfies(version_req, version, package_info)? {
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
  info: &'a NpmPackageInfo,
  tag: &str,
) -> Result<&'a NpmPackageVersionInfo, NpmPackageVersionResolutionError> {
  // For when someone just specifies @types/node, we want to pull in a
  // "known good" version of @types/node that works well with Deno and
  // not necessarily the latest version. For example, we might only be
  // compatible with Node vX, but then Node vY is published so we wouldn't
  // want to pull that in.
  // Note: If the user doesn't want this behavior, then they can specify an
  // explicit version.
  if tag == "latest" && info.name == "@types/node" {
    return get_resolved_package_version_and_info(
      // todo(dsherret): follow up and remove this in a future PR
      // WARNING: When bumping this version, check if anything needs to be
      // updated in the `setNodeOnlyGlobalNames` call in 99_main_compiler.js
      &VersionReq::parse_from_npm("18.0.0 - 18.11.18").unwrap(),
      info,
    );
  }

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

#[cfg(test)]
mod test {
  use std::collections::HashMap;

  use deno_semver::npm::NpmPackageReqReference;

  use super::*;

  #[test]
  fn test_get_resolved_package_version_and_info() {
    // dist tag where version doesn't exist
    let package_ref = NpmPackageReqReference::from_str("npm:test").unwrap();
    let package_info = NpmPackageInfo {
      name: "test".to_string(),
      versions: HashMap::new(),
      dist_tags: HashMap::from([(
        "latest".to_string(),
        Version::parse_from_npm("1.0.0-alpha").unwrap(),
      )]),
    };
    let result = get_resolved_package_version_and_info(
      package_ref
        .req
        .version_req
        .as_ref()
        .unwrap_or(&*LATEST_VERSION_REQ),
      &package_info,
    );
    assert_eq!(
      result.err().unwrap().to_string(),
      "Could not find version '1.0.0-alpha' referenced in dist-tag 'latest' for npm package 'test'."
    );

    // dist tag where version is a pre-release
    let package_ref = NpmPackageReqReference::from_str("npm:test").unwrap();
    let package_info = NpmPackageInfo {
      name: "test".to_string(),
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
        "latest".to_string(),
        Version::parse_from_npm("1.0.0-alpha").unwrap(),
      )]),
    };
    let result = get_resolved_package_version_and_info(
      package_ref
        .req
        .version_req
        .as_ref()
        .unwrap_or(&*LATEST_VERSION_REQ),
      &package_info,
    );
    assert_eq!(result.unwrap().version.to_string(), "1.0.0-alpha");
  }
}
