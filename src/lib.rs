// Copyright 2018-2024 the Deno authors. MIT license.

#![deny(clippy::print_stderr)]
#![deny(clippy::print_stdout)]
#![deny(clippy::unused_async)]

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;

use deno_semver::package::PackageNv;
use deno_semver::Version;
use registry::NpmPackageVersionBinEntry;
use registry::NpmPackageVersionDistInfo;
use resolution::SerializedNpmResolutionSnapshotPackage;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

pub mod npm_rc;
pub mod registry;
pub mod resolution;

#[derive(Debug, Error)]
#[error("Invalid npm package id '{text}'. {message}")]
pub struct NpmPackageIdDeserializationError {
  message: String,
  text: String,
}

/// A resolved unique identifier for an npm package. This contains
/// the resolved name, version, and peer dependency resolution identifiers.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NpmPackageId {
  pub nv: PackageNv,
  pub peer_dependencies: Vec<NpmPackageId>,
}

// Custom debug implementation for more concise test output
impl std::fmt::Debug for NpmPackageId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.as_serialized())
  }
}

impl NpmPackageId {
  pub fn as_serialized(&self) -> String {
    self.as_serialized_with_level(0)
  }

  fn as_serialized_with_level(&self, level: usize) -> String {
    // WARNING: This should not change because it's used in the lockfile
    let mut result = format!(
      "{}@{}",
      if level == 0 {
        self.nv.name.to_string()
      } else {
        self.nv.name.replace('/', "+")
      },
      self.nv.version
    );
    for peer in &self.peer_dependencies {
      // unfortunately we can't do something like `_3` when
      // this gets deep because npm package names can start
      // with a number
      result.push_str(&"_".repeat(level + 1));
      result.push_str(&peer.as_serialized_with_level(level + 1));
    }
    result
  }

  pub fn from_serialized(
    id: &str,
  ) -> Result<Self, NpmPackageIdDeserializationError> {
    use monch::*;

    fn parse_name(input: &str) -> ParseResult<&str> {
      if_not_empty(substring(move |input| {
        for (pos, c) in input.char_indices() {
          // first character might be a scope, so skip it
          if pos > 0 && c == '@' {
            return Ok((&input[pos..], ()));
          }
        }
        ParseError::backtrace()
      }))(input)
    }

    fn parse_version(input: &str) -> ParseResult<&str> {
      if_not_empty(substring(skip_while(|c| c != '_')))(input)
    }

    fn parse_name_and_version(input: &str) -> ParseResult<(String, Version)> {
      let (input, name) = parse_name(input)?;
      let (input, _) = ch('@')(input)?;
      let at_version_input = input;
      let (input, version) = parse_version(input)?;
      // todo: improve monch to provide the error message without source
      match Version::parse_from_npm(version) {
        Ok(version) => Ok((input, (name.to_string(), version))),
        Err(err) => ParseError::fail(
          at_version_input,
          format!("Invalid npm version. {}", err.message()),
        ),
      }
    }

    fn parse_level_at_level<'a>(
      level: usize,
    ) -> impl Fn(&'a str) -> ParseResult<'a, ()> {
      fn parse_level(input: &str) -> ParseResult<usize> {
        let level = input.chars().take_while(|c| *c == '_').count();
        Ok((&input[level..], level))
      }

      move |input| {
        let (input, parsed_level) = parse_level(input)?;
        if parsed_level == level {
          Ok((input, ()))
        } else {
          ParseError::backtrace()
        }
      }
    }

    fn parse_peers_at_level<'a>(
      level: usize,
    ) -> impl Fn(&'a str) -> ParseResult<'a, Vec<NpmPackageId>> {
      move |mut input| {
        let mut peers = Vec::new();
        while let Ok((level_input, _)) = parse_level_at_level(level)(input) {
          input = level_input;
          let peer_result = parse_id_at_level(level)(input)?;
          input = peer_result.0;
          peers.push(peer_result.1);
        }
        Ok((input, peers))
      }
    }

    fn parse_id_at_level<'a>(
      level: usize,
    ) -> impl Fn(&'a str) -> ParseResult<'a, NpmPackageId> {
      move |input| {
        let (input, (name, version)) = parse_name_and_version(input)?;
        let name = if level > 0 {
          name.replace('+', "/")
        } else {
          name
        };
        let (input, peer_dependencies) =
          parse_peers_at_level(level + 1)(input)?;
        Ok((
          input,
          NpmPackageId {
            nv: PackageNv { name, version },
            peer_dependencies,
          },
        ))
      }
    }

    with_failure_handling(parse_id_at_level(0))(id).map_err(|err| {
      NpmPackageIdDeserializationError {
        message: format!("{err:#}"),
        text: id.to_string(),
      }
    })
  }
}

impl Ord for NpmPackageId {
  fn cmp(&self, other: &Self) -> Ordering {
    match self.nv.cmp(&other.nv) {
      Ordering::Equal => self.peer_dependencies.cmp(&other.peer_dependencies),
      ordering => ordering,
    }
  }
}

impl PartialOrd for NpmPackageId {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

/// Represents an npm package as it might be found in a cache folder
/// where duplicate copies of the same package may exist.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NpmPackageCacheFolderId {
  pub nv: PackageNv,
  /// Peer dependency resolution may require us to have duplicate copies
  /// of the same package.
  pub copy_index: u8,
}

impl NpmPackageCacheFolderId {
  pub fn with_no_count(&self) -> Self {
    Self {
      nv: self.nv.clone(),
      copy_index: 0,
    }
  }
}

impl std::fmt::Display for NpmPackageCacheFolderId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.nv)?;
    if self.copy_index > 0 {
      write!(f, "_{}", self.copy_index)?;
    }
    Ok(())
  }
}

#[derive(Default, Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct NpmResolutionPackageSystemInfo {
  pub os: Vec<String>,
  pub cpu: Vec<String>,
}

impl NpmResolutionPackageSystemInfo {
  pub fn matches_system(&self, system_info: &NpmSystemInfo) -> bool {
    self.matches_cpu(&system_info.cpu) && self.matches_os(&system_info.os)
  }

  pub fn matches_cpu(&self, target: &str) -> bool {
    matches_os_or_cpu_vec(&self.cpu, target)
  }

  pub fn matches_os(&self, target: &str) -> bool {
    matches_os_or_cpu_vec(&self.os, target)
  }
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct NpmResolutionPackage {
  pub id: NpmPackageId,
  /// The peer dependency resolution can differ for the same
  /// package (name and version) depending on where it is in
  /// the resolution tree. This copy index indicates which
  /// copy of the package this is.
  pub copy_index: u8,
  #[serde(flatten)]
  pub system: NpmResolutionPackageSystemInfo,
  pub dist: NpmPackageVersionDistInfo,
  /// Key is what the package refers to the other package as,
  /// which could be different from the package name.
  pub dependencies: HashMap<String, NpmPackageId>,
  pub optional_dependencies: HashSet<String>,
  pub bin: Option<NpmPackageVersionBinEntry>,
}

impl std::fmt::Debug for NpmResolutionPackage {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    // custom debug implementation for deterministic output in the tests
    f.debug_struct("NpmResolutionPackage")
      .field("pkg_id", &self.id)
      .field("copy_index", &self.copy_index)
      .field("system", &self.system)
      .field("dist", &self.dist)
      .field(
        "dependencies",
        &self.dependencies.iter().collect::<BTreeMap<_, _>>(),
      )
      .field("optional_dependencies", &{
        let mut deps = self.optional_dependencies.iter().collect::<Vec<_>>();
        deps.sort();
        deps
      })
      .finish()
  }
}

impl NpmResolutionPackage {
  pub fn as_serialized(&self) -> SerializedNpmResolutionSnapshotPackage {
    SerializedNpmResolutionSnapshotPackage {
      id: self.id.clone(),
      system: self.system.clone(),
      dist: self.dist.clone(),
      dependencies: self.dependencies.clone(),
      optional_dependencies: self.optional_dependencies.clone(),
      bin: self.bin.clone(),
    }
  }

  pub fn get_package_cache_folder_id(&self) -> NpmPackageCacheFolderId {
    NpmPackageCacheFolderId {
      nv: self.id.nv.clone(),
      copy_index: self.copy_index,
    }
  }
}

/// System information used to determine which optional packages
/// to download.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct NpmSystemInfo {
  /// `process.platform` value from Node.js
  pub os: String,
  /// `process.arch` value from Node.js
  pub cpu: String,
}

impl Default for NpmSystemInfo {
  fn default() -> Self {
    Self {
      os: node_js_os(std::env::consts::OS),
      cpu: node_js_cpu(std::env::consts::ARCH),
    }
  }
}

impl NpmSystemInfo {
  pub fn from_rust(os: &str, cpu: &str) -> Self {
    Self {
      os: node_js_os(os),
      cpu: node_js_cpu(cpu),
    }
  }
}

fn matches_os_or_cpu_vec(items: &[String], target: &str) -> bool {
  if items.is_empty() {
    return true;
  }
  let mut had_negation = false;
  for item in items {
    if item.starts_with('!') {
      if &item[1..] == target {
        return false;
      }
      had_negation = true;
    } else if item == target {
      return true;
    }
  }
  had_negation
}

fn node_js_cpu(rust_arch: &str) -> String {
  // possible values: https://nodejs.org/api/process.html#processarch
  // 'arm', 'arm64', 'ia32', 'mips','mipsel', 'ppc', 'ppc64', 's390', 's390x', and 'x64'
  match rust_arch {
    "x86_64" => "x64",
    "aarch64" => "arm64",
    value => value,
  }
  .to_string()
}

fn node_js_os(rust_os: &str) -> String {
  // possible values: https://nodejs.org/api/process.html#processplatform
  // 'aix', 'darwin', 'freebsd', 'linux', 'openbsd', 'sunos', and 'win32'
  match rust_os {
    "macos" => "darwin",
    "windows" => "win32",
    value => value,
  }
  .to_string()
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn serialize_npm_package_id() {
    let id = NpmPackageId {
      nv: PackageNv::from_str("pkg-a@1.2.3").unwrap(),
      peer_dependencies: vec![
        NpmPackageId {
          nv: PackageNv::from_str("pkg-b@3.2.1").unwrap(),
          peer_dependencies: vec![
            NpmPackageId {
              nv: PackageNv::from_str("pkg-c@1.3.2").unwrap(),
              peer_dependencies: vec![],
            },
            NpmPackageId {
              nv: PackageNv::from_str("pkg-d@2.3.4").unwrap(),
              peer_dependencies: vec![],
            },
          ],
        },
        NpmPackageId {
          nv: PackageNv::from_str("pkg-e@2.3.1").unwrap(),
          peer_dependencies: vec![NpmPackageId {
            nv: PackageNv::from_str("pkg-f@2.3.1").unwrap(),
            peer_dependencies: vec![],
          }],
        },
      ],
    };

    // this shouldn't change because it's used in the lockfile
    let serialized = id.as_serialized();
    assert_eq!(serialized, "pkg-a@1.2.3_pkg-b@3.2.1__pkg-c@1.3.2__pkg-d@2.3.4_pkg-e@2.3.1__pkg-f@2.3.1");
    assert_eq!(NpmPackageId::from_serialized(&serialized).unwrap(), id);
  }

  #[test]
  fn parse_npm_package_id() {
    #[track_caller]
    fn run_test(input: &str) {
      let id = NpmPackageId::from_serialized(input).unwrap();
      assert_eq!(id.as_serialized(), input);
    }

    run_test("pkg-a@1.2.3");
    run_test("pkg-a@1.2.3_pkg-b@3.2.1");
    run_test(
      "pkg-a@1.2.3_pkg-b@3.2.1__pkg-c@1.3.2__pkg-d@2.3.4_pkg-e@2.3.1__pkg-f@2.3.1",
    );

    #[track_caller]
    fn run_error_test(input: &str, message: &str) {
      let err = NpmPackageId::from_serialized(input).unwrap_err();
      assert_eq!(format!("{:#}", err), message);
    }

    run_error_test(
      "asdf",
      "Invalid npm package id 'asdf'. Unexpected character.
  asdf
  ~",
    );
    run_error_test(
      "asdf@test",
      "Invalid npm package id 'asdf@test'. Invalid npm version. Unexpected character.
  test
  ~",
    );
    run_error_test(
      "pkg@1.2.3_asdf@test",
      "Invalid npm package id 'pkg@1.2.3_asdf@test'. Invalid npm version. Unexpected character.
  test
  ~",
    );
  }

  #[test]
  fn test_matches_os_or_cpu_vec() {
    assert!(matches_os_or_cpu_vec(&[], "x64"));
    assert!(matches_os_or_cpu_vec(&["x64".to_string()], "x64"));
    assert!(!matches_os_or_cpu_vec(&["!x64".to_string()], "x64"));
    assert!(matches_os_or_cpu_vec(&["!arm64".to_string()], "x64"));
    assert!(matches_os_or_cpu_vec(
      &["!arm64".to_string(), "!x86".to_string()],
      "x64"
    ));
    assert!(!matches_os_or_cpu_vec(
      &["!arm64".to_string(), "!x86".to_string()],
      "x86"
    ));
    assert!(!matches_os_or_cpu_vec(
      &[
        "!arm64".to_string(),
        "!x86".to_string(),
        "other".to_string()
      ],
      "x86"
    ));

    // not explicitly excluded and there's an include, so it's considered a match
    assert!(matches_os_or_cpu_vec(
      &[
        "!arm64".to_string(),
        "!x86".to_string(),
        "other".to_string()
      ],
      "x64"
    ));
  }
}
