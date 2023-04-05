use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use deno_semver::npm::NpmPackageNv;
use deno_semver::npm::NpmVersionReqParseError;
use deno_semver::Version;
use deno_semver::VersionReq;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("Could not find @ symbol in npm url '{value}'")]
pub struct PackageDepNpmSchemeValueParseError {
  pub value: String,
}

/// Gets the name and raw version constraint for a registry info or
/// package.json dependency entry taking into account npm package aliases.
pub fn parse_dep_entry_name_and_raw_version<'a>(
  key: &'a str,
  value: &'a str,
) -> Result<(&'a str, &'a str), PackageDepNpmSchemeValueParseError> {
  if let Some(package_and_version) = value.strip_prefix("npm:") {
    if let Some((name, version)) = package_and_version.rsplit_once('@') {
      Ok((name, version))
    } else {
      Err(PackageDepNpmSchemeValueParseError {
        value: value.to_string(),
      })
    }
  } else {
    Ok((key, value))
  }
}

// npm registry docs: https://github.com/npm/registry/blob/master/docs/REGISTRY-API.md

#[derive(Debug, Default, Deserialize, Serialize, Clone)]
pub struct NpmPackageInfo {
  pub name: String,
  pub versions: HashMap<Version, NpmPackageVersionInfo>,
  #[serde(rename = "dist-tags")]
  pub dist_tags: HashMap<String, Version>,
}

#[derive(Debug, Clone, Error)]
#[error(
  "Error parsing version requirement for dependency: {key}@{version_req}\n\n{source:#}"
)]
pub struct NpmDependencyEntryError {
  pub key: String,
  pub version_req: String,
  #[source]
  pub source: NpmDependencyEntryErrorSource,
}

#[derive(Debug, Clone, Error)]
pub enum NpmDependencyEntryErrorSource {
  #[error(transparent)]
  NpmVersionReqParseError(NpmVersionReqParseError),
  #[error(transparent)]
  PackageDepNpmSchemeValueParseError(PackageDepNpmSchemeValueParseError),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NpmDependencyEntryKind {
  Dep,
  Peer,
  OptionalPeer,
}

impl NpmDependencyEntryKind {
  pub fn is_optional(&self) -> bool {
    matches!(self, NpmDependencyEntryKind::OptionalPeer)
  }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NpmDependencyEntry {
  pub kind: NpmDependencyEntryKind,
  pub bare_specifier: String,
  pub name: String,
  pub version_req: VersionReq,
  /// When the dependency is also marked as a peer dependency,
  /// use this entry to resolve the dependency when it can't
  /// be resolved as a peer dependency.
  pub peer_dep_version_req: Option<VersionReq>,
}

impl PartialOrd for NpmDependencyEntry {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for NpmDependencyEntry {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    // sort the dependencies alphabetically by name then by version descending
    match self.name.cmp(&other.name) {
      // sort by newest to oldest
      Ordering::Equal => other
        .version_req
        .version_text()
        .cmp(self.version_req.version_text()),
      ordering => ordering,
    }
  }
}

#[derive(Debug, Default, Deserialize, Serialize, Clone, PartialEq, Eq)]
pub struct NpmPeerDependencyMeta {
  #[serde(default)]
  optional: bool,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq, Eq)]
#[serde(untagged)]
pub enum NpmPackageVersionBinEntry {
  String(String),
  Map(HashMap<String, String>),
}

#[derive(Debug, Default, Deserialize, Serialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct NpmPackageVersionInfo {
  pub version: Version,
  pub dist: NpmPackageVersionDistInfo,
  pub bin: Option<NpmPackageVersionBinEntry>,
  // Bare specifier to version (ex. `"typescript": "^3.0.1") or possibly
  // package and version (ex. `"typescript-3.0.1": "npm:typescript@3.0.1"`).
  #[serde(default)]
  pub dependencies: HashMap<String, String>,
  #[serde(default)]
  pub peer_dependencies: HashMap<String, String>,
  #[serde(default)]
  pub peer_dependencies_meta: HashMap<String, NpmPeerDependencyMeta>,
}

impl NpmPackageVersionInfo {
  pub fn dependencies_as_entries(
    &self,
  ) -> Result<Vec<NpmDependencyEntry>, NpmDependencyEntryError> {
    fn parse_dep_entry_inner(
      (key, value): (&String, &String),
      kind: NpmDependencyEntryKind,
    ) -> Result<NpmDependencyEntry, NpmDependencyEntryErrorSource> {
      let (name, version_req) =
        parse_dep_entry_name_and_raw_version(key, value).map_err(
          NpmDependencyEntryErrorSource::PackageDepNpmSchemeValueParseError,
        )?;
      let version_req = VersionReq::parse_from_npm(version_req)
        .map_err(NpmDependencyEntryErrorSource::NpmVersionReqParseError)?;
      Ok(NpmDependencyEntry {
        kind,
        bare_specifier: key.to_string(),
        name: name.to_string(),
        version_req,
        peer_dep_version_req: None,
      })
    }

    fn parse_dep_entry(
      key_value: (&String, &String),
      kind: NpmDependencyEntryKind,
    ) -> Result<NpmDependencyEntry, NpmDependencyEntryError> {
      parse_dep_entry_inner(key_value, kind).map_err(|source| {
        NpmDependencyEntryError {
          key: key_value.0.to_string(),
          version_req: key_value.1.to_string(),
          source,
        }
      })
    }

    let mut result = HashMap::with_capacity(
      self.dependencies.len() + self.peer_dependencies.len(),
    );
    for entry in &self.peer_dependencies {
      let is_optional = self
        .peer_dependencies_meta
        .get(entry.0)
        .map(|d| d.optional)
        .unwrap_or(false);
      let kind = match is_optional {
        true => NpmDependencyEntryKind::OptionalPeer,
        false => NpmDependencyEntryKind::Peer,
      };
      let entry = parse_dep_entry(entry, kind)?;
      result.insert(entry.bare_specifier.clone(), entry);
    }
    for entry in &self.dependencies {
      let entry = parse_dep_entry(entry, NpmDependencyEntryKind::Dep)?;
      // people may define a dependency as a peer dependency as well,
      // so in those cases, attempt to resolve as a peer dependency,
      // but then use this dependency version requirement otherwise
      if let Some(peer_dep_entry) = result.get_mut(&entry.bare_specifier) {
        peer_dep_entry.peer_dep_version_req = Some(entry.version_req);
      } else {
        result.insert(entry.bare_specifier.clone(), entry);
      }
    }
    Ok(result.into_values().collect())
  }
}

#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct NpmPackageVersionDistInfo {
  /// URL to the tarball.
  pub tarball: String,
  shasum: String,
  integrity: Option<String>,
}

impl NpmPackageVersionDistInfo {
  pub fn integrity(&self) -> Cow<String> {
    self
      .integrity
      .as_ref()
      .map(Cow::Borrowed)
      .unwrap_or_else(|| Cow::Owned(format!("sha1-{}", self.shasum)))
  }
}

// todo(dsherret): remove `Sync` here and use `async_trait(?Send)` once the LSP
// in the Deno repo is no longer `Send` (https://github.com/denoland/deno/issues/18079)
#[async_trait]
pub trait NpmRegistryApi: Sync {
  /// Gets the package information from the npm registry.
  ///
  /// Note: The implementer should handle requests for the same npm
  /// package name concurrently and try not to make the same request
  /// to npm at the same time.
  async fn maybe_package_info(
    &self,
    name: &str,
  ) -> Result<Option<Arc<NpmPackageInfo>>, anyhow::Error>;

  async fn package_info(
    &self,
    name: &str,
  ) -> Result<Arc<NpmPackageInfo>, anyhow::Error> {
    let maybe_package_info = self.maybe_package_info(name).await?;
    match maybe_package_info {
      Some(package_info) => Ok(package_info),
      None => anyhow::bail!("npm package '{}' does not exist", name),
    }
  }

  async fn package_version_info(
    &self,
    nv: &NpmPackageNv,
  ) -> Result<Option<NpmPackageVersionInfo>, anyhow::Error> {
    let package_info = self.package_info(&nv.name).await?;
    Ok(package_info.versions.get(&nv.version).cloned())
  }
}

/// Note: This test struct is not thread safe for setup
/// purposes. Construct everything on the same thread.
#[cfg(test)]
#[derive(Clone, Default, Debug)]
pub struct TestNpmRegistryApi {
  package_infos: Arc<parking_lot::Mutex<HashMap<String, NpmPackageInfo>>>,
}

#[cfg(test)]
impl TestNpmRegistryApi {
  pub fn add_package_info(&self, name: &str, info: NpmPackageInfo) {
    let previous = self.package_infos.lock().insert(name.to_string(), info);
    assert!(previous.is_none());
  }

  pub fn ensure_package(&self, name: &str) {
    if !self.package_infos.lock().contains_key(name) {
      self.add_package_info(
        name,
        NpmPackageInfo {
          name: name.to_string(),
          ..Default::default()
        },
      );
    }
  }

  pub fn ensure_package_version(&self, name: &str, version: &str) {
    self.ensure_package(name);
    let mut infos = self.package_infos.lock();
    let info = infos.get_mut(name).unwrap();
    let version = Version::parse_from_npm(version).unwrap();
    if !info.versions.contains_key(&version) {
      info.versions.insert(
        version.clone(),
        NpmPackageVersionInfo {
          version,
          ..Default::default()
        },
      );
    }
  }

  pub fn add_dependency(
    &self,
    package_from: (&str, &str),
    package_to: (&str, &str),
  ) {
    let mut infos = self.package_infos.lock();
    let info = infos.get_mut(package_from.0).unwrap();
    let package_from = (
      package_from.0,
      Version::parse_from_npm(package_from.1).unwrap(),
    );
    let version = info.versions.get_mut(&package_from.1).unwrap();
    version
      .dependencies
      .insert(package_to.0.to_string(), package_to.1.to_string());
  }

  pub fn add_dist_tag(&self, package_name: &str, tag: &str, version: &str) {
    let mut infos = self.package_infos.lock();
    let info = infos.get_mut(package_name).unwrap();
    info
      .dist_tags
      .insert(tag.to_string(), Version::parse_from_npm(version).unwrap());
  }

  pub fn add_peer_dependency(
    &self,
    package_from: (&str, &str),
    package_to: (&str, &str),
  ) {
    let package_from = (
      package_from.0,
      Version::parse_from_npm(package_from.1).unwrap(),
    );
    let mut infos = self.package_infos.lock();
    let info = infos.get_mut(package_from.0).unwrap();
    let version = info.versions.get_mut(&package_from.1).unwrap();
    version
      .peer_dependencies
      .insert(package_to.0.to_string(), package_to.1.to_string());
  }

  pub fn add_optional_peer_dependency(
    &self,
    package_from: (&str, &str),
    package_to: (&str, &str),
  ) {
    let package_from = (
      package_from.0,
      Version::parse_from_npm(package_from.1).unwrap(),
    );
    let mut infos = self.package_infos.lock();
    let info = infos.get_mut(package_from.0).unwrap();
    let version = info.versions.get_mut(&package_from.1).unwrap();
    version
      .peer_dependencies
      .insert(package_to.0.to_string(), package_to.1.to_string());
    version.peer_dependencies_meta.insert(
      package_to.0.to_string(),
      NpmPeerDependencyMeta { optional: true },
    );
  }
}

#[cfg(test)]
#[async_trait]
impl NpmRegistryApi for TestNpmRegistryApi {
  async fn maybe_package_info(
    &self,
    name: &str,
  ) -> Result<Option<Arc<NpmPackageInfo>>, anyhow::Error> {
    let infos = self.package_infos.lock();
    Ok(infos.get(name).cloned().map(Arc::new))
  }
}

#[cfg(test)]
mod test {
  use std::collections::HashMap;

  use deno_semver::Version;
  use serde_json;

  use super::NpmPackageVersionBinEntry;
  use super::NpmPackageVersionDistInfo;
  use super::NpmPackageVersionInfo;

  #[test]
  fn deserializes_minimal_pkg_info() {
    let text = r#"{ "version": "1.0.0", "dist": { "tarball": "value", "shasum": "test" } }"#;
    let info: NpmPackageVersionInfo = serde_json::from_str(text).unwrap();
    assert_eq!(
      info,
      NpmPackageVersionInfo {
        version: Version::parse_from_npm("1.0.0").unwrap(),
        dist: NpmPackageVersionDistInfo {
          tarball: "value".to_string(),
          shasum: "test".to_string(),
          integrity: None,
        },
        bin: None,
        dependencies: Default::default(),
        peer_dependencies: Default::default(),
        peer_dependencies_meta: Default::default()
      }
    );
  }

  #[test]
  fn deserializes_bin_entry() {
    // string
    let text = r#"{ "version": "1.0.0", "bin": "bin-value", "dist": { "tarball": "value", "shasum": "test" } }"#;
    let info: NpmPackageVersionInfo = serde_json::from_str(text).unwrap();
    assert_eq!(
      info.bin,
      Some(NpmPackageVersionBinEntry::String("bin-value".to_string()))
    );

    // map
    let text = r#"{ "version": "1.0.0", "bin": { "a": "a-value", "b": "b-value" }, "dist": { "tarball": "value", "shasum": "test" } }"#;
    let info: NpmPackageVersionInfo = serde_json::from_str(text).unwrap();
    assert_eq!(
      info.bin,
      Some(NpmPackageVersionBinEntry::Map(HashMap::from([
        ("a".to_string(), "a-value".to_string()),
        ("b".to_string(), "b-value".to_string()),
      ])))
    );
  }
}
