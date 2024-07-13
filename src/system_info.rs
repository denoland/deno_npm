// Copyright 2018-2024 the Deno authors. MIT license.

use serde::Deserialize;
use serde::Serialize;

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

pub(crate) fn matches_os_or_cpu_vec(items: &[String], target: &str) -> bool {
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
