// Copyright 2018-2024 the Deno authors. MIT license.

use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;

use async_trait::async_trait;
use deno_semver::npm::NpmVersionReqParseError;
use deno_semver::package::PackageNv;
use deno_semver::Version;
use deno_semver::VersionReq;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

use crate::resolution::NpmPackageVersionNotFound;

/// Gets the name and raw version constraint for a registry info or
/// package.json dependency entry taking into account npm package aliases.
pub fn parse_dep_entry_name_and_raw_version<'a>(
  key: &'a str,
  value: &'a str,
) -> (&'a str, &'a str) {
  if let Some(package_and_version) = value.strip_prefix("npm:") {
    if let Some((name, version)) = package_and_version.rsplit_once('@') {
      // if empty, then the name was scoped and there's no version
      if name.is_empty() {
        (package_and_version, "*")
      } else {
        (name, version)
      }
    } else {
      (package_and_version, "*")
    }
  } else {
    (key, value)
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

impl NpmPackageInfo {
  pub fn version_info(
    &self,
    nv: &PackageNv,
  ) -> Result<NpmPackageVersionInfo, NpmPackageVersionNotFound> {
    match self.versions.get(&nv.version).cloned() {
      Some(version_info) => Ok(version_info),
      None => Err(NpmPackageVersionNotFound(nv.clone())),
    }
  }
}

#[derive(Debug, Clone, Error)]
#[error(
  "Error in {parent_nv} parsing version requirement for dependency: {key}@{version_req}\n\n{source:#}"
)]
pub struct NpmDependencyEntryError {
  /// Name and version of the package that has this dependency.
  pub parent_nv: PackageNv,
  /// Bare specifier.
  pub key: String,
  /// Version requirement text.
  pub version_req: String,
  #[source]
  pub source: NpmDependencyEntryErrorSource,
}

#[derive(Debug, Clone, Error)]
pub enum NpmDependencyEntryErrorSource {
  #[error(transparent)]
  NpmVersionReqParseError(#[from] NpmVersionReqParseError),
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
  #[serde(deserialize_with = "deserialize_null_default")]
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
  #[serde(deserialize_with = "deserialize_null_default")]
  pub dependencies: HashMap<String, String>,
  #[serde(default)]
  #[serde(deserialize_with = "deserialize_null_default")]
  pub optional_dependencies: HashMap<String, String>,
  #[serde(default)]
  #[serde(deserialize_with = "deserialize_null_default")]
  pub peer_dependencies: HashMap<String, String>,
  #[serde(default)]
  #[serde(deserialize_with = "deserialize_null_default")]
  pub peer_dependencies_meta: HashMap<String, NpmPeerDependencyMeta>,
  #[serde(default)]
  #[serde(deserialize_with = "deserialize_null_default")]
  pub os: Vec<String>,
  #[serde(default)]
  #[serde(deserialize_with = "deserialize_null_default")]
  pub cpu: Vec<String>,
  #[serde(default)]
  #[serde(deserialize_with = "deserialize_null_default")]
  pub scripts: HashMap<String, String>,
}

impl NpmPackageVersionInfo {
  pub fn dependencies_as_entries(
    &self,
    // name of the package used to improve error messages
    package_name: &str,
  ) -> Result<Vec<NpmDependencyEntry>, Box<NpmDependencyEntryError>> {
    fn parse_dep_entry_inner(
      (key, value): (&String, &String),
      kind: NpmDependencyEntryKind,
    ) -> Result<NpmDependencyEntry, NpmDependencyEntryErrorSource> {
      let (name, version_req) =
        parse_dep_entry_name_and_raw_version(key, value);
      let version_req = VersionReq::parse_from_npm(version_req)?;
      Ok(NpmDependencyEntry {
        kind,
        bare_specifier: key.to_string(),
        name: name.to_string(),
        version_req,
        peer_dep_version_req: None,
      })
    }

    fn parse_dep_entry(
      nv: (&str, &Version),
      key_value: (&String, &String),
      kind: NpmDependencyEntryKind,
    ) -> Result<NpmDependencyEntry, Box<NpmDependencyEntryError>> {
      parse_dep_entry_inner(key_value, kind).map_err(|source| {
        Box::new(NpmDependencyEntryError {
          parent_nv: PackageNv {
            name: nv.0.to_string(),
            version: nv.1.clone(),
          },
          key: key_value.0.to_string(),
          version_req: key_value.1.to_string(),
          source,
        })
      })
    }

    let normalized_dependencies = if self
      .optional_dependencies
      .keys()
      .all(|k| self.dependencies.contains_key(k))
    {
      Cow::Borrowed(&self.dependencies)
    } else {
      // Most package information has the optional dependencies duplicated
      // in the dependencies list, but some don't. In those cases, add
      // the optonal dependencies into the map of dependencies
      Cow::Owned(
        self
          .optional_dependencies
          .iter()
          // prefer what's in the dependencies map
          .chain(self.dependencies.iter())
          .map(|(k, v)| (k.clone(), v.clone()))
          .collect(),
      )
    };

    let mut result = HashMap::with_capacity(
      normalized_dependencies.len() + self.peer_dependencies.len(),
    );
    let nv = (package_name, &self.version);
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
      let entry = parse_dep_entry(nv, entry, kind)?;
      result.insert(entry.bare_specifier.clone(), entry);
    }
    for entry in normalized_dependencies.iter() {
      let entry = parse_dep_entry(nv, entry, NpmDependencyEntryKind::Dep)?;
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NpmPackageVersionDistInfoIntegrity<'a> {
  /// A string in the form `sha1-<hash>` where the hash is base64 encoded.
  Integrity {
    algorithm: &'a str,
    base64_hash: &'a str,
  },
  /// The integrity could not be determined because it did not contain a dash.
  UnknownIntegrity(&'a str),
  /// The legacy sha1 hex hash (ex. "62afbee2ffab5e0db139450767a6125cbea50fa2").
  LegacySha1Hex(&'a str),
}

impl<'a> NpmPackageVersionDistInfoIntegrity<'a> {
  pub fn for_lockfile(&self) -> String {
    match self {
      NpmPackageVersionDistInfoIntegrity::Integrity {
        algorithm,
        base64_hash,
      } => format!("{}-{}", algorithm, base64_hash),
      NpmPackageVersionDistInfoIntegrity::UnknownIntegrity(integrity) => {
        integrity.to_string()
      }
      NpmPackageVersionDistInfoIntegrity::LegacySha1Hex(hex) => hex.to_string(),
    }
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
  pub fn integrity(&self) -> NpmPackageVersionDistInfoIntegrity {
    match &self.integrity {
      Some(integrity) => match integrity.split_once('-') {
        Some((algorithm, base64_hash)) => {
          NpmPackageVersionDistInfoIntegrity::Integrity {
            algorithm,
            base64_hash,
          }
        }
        None => NpmPackageVersionDistInfoIntegrity::UnknownIntegrity(
          integrity.as_str(),
        ),
      },
      None => NpmPackageVersionDistInfoIntegrity::LegacySha1Hex(&self.shasum),
    }
  }
}

/// Error that occurs when loading the package info from the npm registry fails.
#[derive(Debug, Error, Clone)]
pub enum NpmRegistryPackageInfoLoadError {
  #[error("npm package '{package_name}' does not exist.")]
  PackageNotExists { package_name: String },
  #[error(transparent)]
  LoadError(#[from] Arc<anyhow::Error>),
}

// todo(dsherret): remove `Sync` here and use `async_trait(?Send)` once the LSP
// in the Deno repo is no longer `Send` (https://github.com/denoland/deno/issues/18079)
/// A trait for getting package information from the npm registry.
///
/// An implementer may want to override the default implementation of
/// [`mark_force_reload`] method if it has a cache mechanism.
#[async_trait]
pub trait NpmRegistryApi: Sync + Send {
  /// Gets the package information from the npm registry.
  ///
  /// Note: The implementer should handle requests for the same npm
  /// package name concurrently and try not to make the same request
  /// to npm at the same time.
  async fn package_info(
    &self,
    name: &str,
  ) -> Result<Arc<NpmPackageInfo>, NpmRegistryPackageInfoLoadError>;

  /// Marks that new requests for package information should retrieve it
  /// from the npm registry
  ///
  /// Returns true if both of the following conditions are met:
  /// - the implementer has a cache mechanism
  /// - "force reload" flag is successfully set for the first time
  fn mark_force_reload(&self) -> bool {
    false
  }
}

/// A simple in-memory implementation of the NpmRegistryApi
/// that can be used for testing purposes. This does not use
/// `#[cfg(test)]` because that is not supported across crates.
///
/// Note: This test struct is not thread safe for setup
/// purposes. Construct everything on the same thread.
#[derive(Clone, Default, Debug)]
pub struct TestNpmRegistryApi {
  package_infos: Arc<Mutex<HashMap<String, NpmPackageInfo>>>,
}

impl TestNpmRegistryApi {
  pub fn add_package_info(&self, name: &str, info: NpmPackageInfo) {
    let previous = self
      .package_infos
      .lock()
      .unwrap()
      .insert(name.to_string(), info);
    assert!(previous.is_none());
  }

  pub fn ensure_package(&self, name: &str) {
    if !self.package_infos.lock().unwrap().contains_key(name) {
      self.add_package_info(
        name,
        NpmPackageInfo {
          name: name.to_string(),
          ..Default::default()
        },
      );
    }
  }

  pub fn with_package(&self, name: &str, f: impl FnOnce(&mut NpmPackageInfo)) {
    self.ensure_package(name);
    let mut infos = self.package_infos.lock().unwrap();
    let info = infos.get_mut(name).unwrap();
    f(info);
  }

  pub fn add_dist_tag(&self, package_name: &str, tag: &str, version: &str) {
    self.with_package(package_name, |package| {
      package
        .dist_tags
        .insert(tag.to_string(), Version::parse_from_npm(version).unwrap());
    })
  }

  pub fn ensure_package_version(&self, name: &str, version: &str) {
    self.ensure_package_version_with_integrity(name, version, None)
  }

  pub fn ensure_package_version_with_integrity(
    &self,
    name: &str,
    version: &str,
    integrity: Option<&str>,
  ) {
    self.ensure_package(name);
    let mut infos = self.package_infos.lock().unwrap();
    let info = infos.get_mut(name).unwrap();
    let version = Version::parse_from_npm(version).unwrap();
    if !info.versions.contains_key(&version) {
      info.versions.insert(
        version.clone(),
        NpmPackageVersionInfo {
          version,
          dist: NpmPackageVersionDistInfo {
            integrity: integrity.map(|s| s.to_string()),
            ..Default::default()
          },
          ..Default::default()
        },
      );
    }
  }

  pub fn with_version_info(
    &self,
    package: (&str, &str),
    f: impl FnOnce(&mut NpmPackageVersionInfo),
  ) {
    let (name, version) = package;
    self.ensure_package_version(name, version);
    let mut infos = self.package_infos.lock().unwrap();
    let info = infos.get_mut(name).unwrap();
    let version = Version::parse_from_npm(version).unwrap();
    let version_info = info.versions.get_mut(&version).unwrap();
    f(version_info);
  }

  pub fn add_dependency(&self, package: (&str, &str), entry: (&str, &str)) {
    self.with_version_info(package, |version| {
      version
        .dependencies
        .insert(entry.0.to_string(), entry.1.to_string());
    })
  }

  pub fn add_dep_and_optional_dep(
    &self,
    package: (&str, &str),
    entry: (&str, &str),
  ) {
    self.with_version_info(package, |version| {
      version
        .dependencies
        .insert(entry.0.to_string(), entry.1.to_string());
      version
        .optional_dependencies
        .insert(entry.0.to_string(), entry.1.to_string());
    })
  }

  pub fn add_optional_dep(&self, package: (&str, &str), entry: (&str, &str)) {
    self.with_version_info(package, |version| {
      version
        .optional_dependencies
        .insert(entry.0.to_string(), entry.1.to_string());
    })
  }

  pub fn add_peer_dependency(
    &self,
    package: (&str, &str),
    entry: (&str, &str),
  ) {
    self.with_version_info(package, |version| {
      version
        .peer_dependencies
        .insert(entry.0.to_string(), entry.1.to_string());
    });
  }

  pub fn add_optional_peer_dependency(
    &self,
    package: (&str, &str),
    entry: (&str, &str),
  ) {
    self.with_version_info(package, |version| {
      version
        .peer_dependencies
        .insert(entry.0.to_string(), entry.1.to_string());
      version.peer_dependencies_meta.insert(
        entry.0.to_string(),
        NpmPeerDependencyMeta { optional: true },
      );
    });
  }
}

#[async_trait]
impl NpmRegistryApi for TestNpmRegistryApi {
  async fn package_info(
    &self,
    name: &str,
  ) -> Result<Arc<NpmPackageInfo>, NpmRegistryPackageInfoLoadError> {
    let infos = self.package_infos.lock().unwrap();
    Ok(Arc::new(
      infos
        .get(name)
        .cloned()
        .unwrap_or_else(|| panic!("Not found: {name}")),
    ))
  }
}

/// Deserializes empty or null values to the default value (npm allows uploading
/// `null` for values and serde doesn't automatically make that the default).
///
/// Code from: https://github.com/serde-rs/serde/issues/1098#issuecomment-760711617
fn deserialize_null_default<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
  T: Default + Deserialize<'de>,
  D: serde::Deserializer<'de>,
{
  let opt = Option::deserialize(deserializer)?;
  Ok(opt.unwrap_or_default())
}

#[cfg(test)]
mod test {
  use std::collections::HashMap;

  use deno_semver::Version;
  use serde_json;

  use super::*;

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
        ..Default::default()
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

  #[test]
  fn deserializes_null_entries() {
    let text = r#"{ "version": "1.0.0", "dist": { "tarball": "value", "shasum": "test" }, "dependencies": null, "optionalDependencies": null, "peerDependencies": null, "peerDependenciesMeta": null, "os": null, "cpu": null, "scripts": null }"#;
    let info: NpmPackageVersionInfo = serde_json::from_str(text).unwrap();
    assert!(info.dependencies.is_empty());
    assert!(info.optional_dependencies.is_empty());
    assert!(info.peer_dependencies.is_empty());
    assert!(info.peer_dependencies_meta.is_empty());
    assert!(info.os.is_empty());
    assert!(info.cpu.is_empty());
    assert!(info.scripts.is_empty());
  }

  #[test]
  fn itegrity() {
    // integrity
    let text =
      r#"{ "tarball": "", "integrity": "sha512-testing", "shasum": "here" }"#;
    let info: NpmPackageVersionDistInfo = serde_json::from_str(text).unwrap();
    assert_eq!(
      info.integrity(),
      super::NpmPackageVersionDistInfoIntegrity::Integrity {
        algorithm: "sha512",
        base64_hash: "testing"
      }
    );

    // no integrity
    let text = r#"{ "tarball": "", "shasum": "here" }"#;
    let info: NpmPackageVersionDistInfo = serde_json::from_str(text).unwrap();
    assert_eq!(
      info.integrity(),
      super::NpmPackageVersionDistInfoIntegrity::LegacySha1Hex("here")
    );

    // no dash
    let text = r#"{ "tarball": "", "integrity": "test", "shasum": "here" }"#;
    let info: NpmPackageVersionDistInfo = serde_json::from_str(text).unwrap();
    assert_eq!(
      info.integrity(),
      super::NpmPackageVersionDistInfoIntegrity::UnknownIntegrity("test")
    );
  }

  #[test]
  fn test_parse_dep_entry_name_and_raw_version() {
    let cases = [
      ("test", "^1.2", ("test", "^1.2")),
      ("test", "1.x - 2.6", ("test", "1.x - 2.6")),
      ("test", "npm:package@^1.2", ("package", "^1.2")),
      ("test", "npm:package", ("package", "*")),
      ("test", "npm:@scope/package", ("@scope/package", "*")),
      ("test", "npm:@scope/package@1", ("@scope/package", "1")),
    ];
    for (key, value, expected_result) in cases {
      let result = parse_dep_entry_name_and_raw_version(key, value);
      assert_eq!(result, expected_result);
    }
  }
}
