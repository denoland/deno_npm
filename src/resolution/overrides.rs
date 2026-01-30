// Copyright 2018-2024 the Deno authors. MIT license.

use std::borrow::Cow;
use std::collections::HashMap;

use deno_semver::StackString;
use deno_semver::Version;
use deno_semver::VersionReq;
use deno_semver::package::PackageName;
use thiserror::Error;

/// The value an override resolves to.
#[derive(Debug, Clone)]
pub enum NpmOverrideValue {
  /// A version requirement, e.g. "1.0.0" or "^2.0.0".
  Version(VersionReq),
  /// No self-override (only child overrides exist). This corresponds to the
  /// case where the override key maps to an object without a "." key.
  Inherited,
  /// The override was explicitly removed (empty string `""`). This cancels
  /// any outer override for the same package within a scoped context.
  Removed,
}

/// A single override rule parsed from the root package.json "overrides" field.
///
/// Represents entries like:
/// - `"foo": "1.0.0"` (simple, no selector, no children)
/// - `"foo@^2.0.0": { ".": "2.1.0", "bar": "3.0.0" }` (selector + dot + children)
#[derive(Debug, Clone)]
pub struct NpmOverrideRule {
  /// The package name this override targets.
  pub name: PackageName,
  /// Optional version range selector on the key (e.g. `^2.0.0` from
  /// `foo@^2.0.0`). When present, the override only applies when the
  /// resolved version satisfies this range.
  pub selector: Option<VersionReq>,
  /// The override value for this package itself.
  pub value: NpmOverrideValue,
  /// Nested overrides that apply within this package's dependency subtree.
  pub children: Vec<NpmOverrideRule>,
}

/// Top-level parsed overrides from the root package.json.
#[derive(Debug, Clone, Default)]
pub struct NpmOverrides {
  pub rules: Vec<NpmOverrideRule>,
}

#[derive(Debug, Error)]
pub enum NpmOverridesError {
  #[error("Failed to parse override key \"{key}\": {source}")]
  KeyParse {
    key: String,
    source: deno_semver::npm::NpmVersionReqParseError,
  },
  #[error(
    "Failed to parse override value \"{value}\" for key \"{key}\": {source}"
  )]
  ValueParse {
    key: String,
    value: String,
    source: deno_semver::npm::NpmVersionReqParseError,
  },
  #[error(
    "Override uses dollar reference \"${reference}\" but \"{reference}\" is not a direct dependency of the root package"
  )]
  UnresolvedDollarReference { reference: String },
  #[error(
    "Invalid override value type for key \"{key}\": expected a string or object"
  )]
  InvalidValueType { key: String },
  #[error(
    "Invalid value type for \".\" key in override \"{key}\": expected a string"
  )]
  InvalidDotValueType { key: String },
}

impl NpmOverrides {
  /// Parses overrides from the raw JSON "overrides" object in package.json.
  ///
  /// `root_deps` should contain all direct dependencies of the root package
  /// (merged from `dependencies`, `devDependencies`, etc.) mapping bare
  /// specifier to version string. This is used to resolve `$pkg` references.
  pub fn from_value(
    value: serde_json::Value,
    root_deps: &HashMap<StackString, StackString>,
  ) -> Result<Self, NpmOverridesError> {
    match value {
      serde_json::Value::Object(map) => {
        let rules = parse_override_rules(&map, root_deps)?;
        Ok(Self { rules })
      }
      serde_json::Value::Null => Ok(Self::default()),
      _ => Ok(Self::default()),
    }
  }

  pub fn is_empty(&self) -> bool {
    self.rules.is_empty()
  }
}

/// Tracks which override rules are applicable at a given point during
/// dependency graph traversal. As resolution descends into a package
/// that matches a scoped override, that rule's children become active.
#[derive(Debug, Clone, Default)]
pub struct ActiveOverrides {
  rules: Vec<NpmOverrideRule>,
}

impl ActiveOverrides {
  /// Creates the initial active overrides from the top-level overrides config.
  pub fn from_overrides(overrides: &NpmOverrides) -> Self {
    if overrides.is_empty() {
      return Self::default();
    }
    Self {
      rules: overrides.rules.clone(),
    }
  }

  pub fn is_empty(&self) -> bool {
    self.rules.is_empty()
  }

  /// Computes the active overrides for a child node in the dependency tree.
  ///
  /// When descending into `child_name@child_version`:
  /// - Rules that target `child_name` and whose selector matches: their
  ///   `children` become active for this subtree. Scoped children are placed
  ///   before passthrough rules so they take precedence in lookups.
  /// - Rules that target other packages (either simple or scoped): pass
  ///   through unchanged so they can match deeper descendants.
  pub fn for_child(
    &self,
    child_name: &PackageName,
    child_version: &Version,
  ) -> Cow<'_, Self> {
    if self.rules.is_empty() {
      return Cow::Borrowed(self);
    }

    let mut scoped_children = Vec::new();
    let mut passthrough_rules = Vec::new();
    let mut changed = false;

    for rule in self.rules.iter() {
      if rule.name == *child_name {
        let selector_matches = match &rule.selector {
          Some(selector) => selector.matches(child_version),
          None => true,
        };

        if selector_matches && !rule.children.is_empty() {
          // this rule targets the current child and matches; activate its
          // nested overrides for this subtree
          scoped_children.extend(rule.children.iter().cloned());
        }
        // whether or not the selector matched, we don't pass this rule
        // through because it was "consumed" by matching or not matching
        // the child's name
        changed = true;
      } else {
        // this rule targets a different package — keep it active for
        // deeper descendants
        passthrough_rules.push(rule.clone());
      }
    }

    if !changed {
      Cow::Borrowed(self)
    } else {
      // scoped children first so they take precedence over passthrough rules
      scoped_children.extend(passthrough_rules);
      Cow::Owned(Self {
        rules: scoped_children,
      })
    }
  }

  /// Looks up an override for a dependency with the given name.
  ///
  /// When `resolved_version` is `None`, only returns overrides without
  /// selectors (unconditional overrides). When `resolved_version` is
  /// `Some`, also checks selector-based overrides against the version.
  ///
  /// Returns the overridden `VersionReq` if an override applies, or `None`
  /// if the dependency should use its original version requirement.
  /// A `Removed` rule causes an immediate `None` return, cancelling any
  /// further override for this dependency.
  pub fn get_override_for(
    &self,
    dep_name: &PackageName,
    resolved_version: Option<&Version>,
  ) -> Option<&VersionReq> {
    for rule in self.rules.iter() {
      if rule.name != *dep_name {
        continue;
      }
      match &rule.value {
        NpmOverrideValue::Version(req) => match &rule.selector {
          None => return Some(req),
          Some(selector) => {
            if let Some(version) = resolved_version
              && selector.matches(version)
            {
              return Some(req);
            }
            // has selector but no version provided or version doesn't
            // match — skip this rule
          }
        },
        NpmOverrideValue::Removed => {
          // explicit removal cancels any override for this dep
          return None;
        }
        NpmOverrideValue::Inherited => {
          // no self-override, only children — skip
        }
      }
    }
    None
  }
}

/// Parses a key from the overrides map. The key can be either:
/// - `"package-name"` → (name, None)
/// - `"package-name@version-req"` → (name, Some(version_req))
fn parse_override_key(
  key: &str,
) -> Result<(PackageName, Option<VersionReq>), NpmOverridesError> {
  // handle scoped packages: "@scope/name" or "@scope/name@version"
  let at_index = if let Some(rest) = key.strip_prefix('@') {
    // scoped package — find the second '@' if it exists
    rest.find('@').map(|i| i + 1)
  } else {
    key.find('@')
  };

  match at_index {
    Some(idx) => {
      let name = &key[..idx];
      let version_text = &key[idx + 1..];
      let version_req =
        VersionReq::parse_from_npm(version_text).map_err(|source| {
          NpmOverridesError::KeyParse {
            key: key.to_string(),
            source,
          }
        })?;
      Ok((PackageName::from_str(name), Some(version_req)))
    }
    None => Ok((PackageName::from_str(key), None)),
  }
}

/// Parses an override value, which can be:
/// - A string: version requirement, `$pkg` reference, or `""` (removal)
/// - An object: nested overrides (with optional "." key for self-override)
fn parse_override_value(
  key: &str,
  value: &serde_json::Value,
  root_deps: &HashMap<StackString, StackString>,
) -> Result<(NpmOverrideValue, Vec<NpmOverrideRule>), NpmOverridesError> {
  match value {
    serde_json::Value::String(s) => {
      if s.is_empty() {
        Ok((NpmOverrideValue::Removed, Vec::new()))
      } else {
        let version_req = resolve_override_version_string(key, s, root_deps)?;
        Ok((NpmOverrideValue::Version(version_req), Vec::new()))
      }
    }
    serde_json::Value::Object(map) => {
      let mut self_value = NpmOverrideValue::Inherited;
      let mut children = Vec::new();

      for (child_key, child_value) in map {
        if child_key == "." {
          // the "." key overrides the package itself
          match child_value {
            serde_json::Value::String(s) => {
              if s.is_empty() {
                self_value = NpmOverrideValue::Removed;
              } else {
                self_value = NpmOverrideValue::Version(
                  resolve_override_version_string(key, s, root_deps)?,
                );
              }
            }
            _ => {
              return Err(NpmOverridesError::InvalidDotValueType {
                key: key.to_string(),
              });
            }
          }
        } else {
          let (child_name, child_selector) = parse_override_key(child_key)?;
          let (child_val, grandchildren) =
            parse_override_value(child_key, child_value, root_deps)?;
          children.push(NpmOverrideRule {
            name: child_name,
            selector: child_selector,
            value: child_val,
            children: grandchildren,
          });
        }
      }

      Ok((self_value, children))
    }
    _ => Err(NpmOverridesError::InvalidValueType {
      key: key.to_string(),
    }),
  }
}

/// Resolves a version string value, handling `$pkg` dollar references.
fn resolve_override_version_string(
  key: &str,
  value: &str,
  root_deps: &HashMap<StackString, StackString>,
) -> Result<VersionReq, NpmOverridesError> {
  if let Some(ref_name) = value.strip_prefix('$') {
    // dollar reference: look up the root dependency's version
    let dep_version = root_deps.get(ref_name).ok_or_else(|| {
      NpmOverridesError::UnresolvedDollarReference {
        reference: ref_name.to_string(),
      }
    })?;
    VersionReq::parse_from_npm(dep_version).map_err(|source| {
      NpmOverridesError::ValueParse {
        key: key.to_string(),
        value: dep_version.to_string(),
        source,
      }
    })
  } else {
    VersionReq::parse_from_npm(value).map_err(|source| {
      NpmOverridesError::ValueParse {
        key: key.to_string(),
        value: value.to_string(),
        source,
      }
    })
  }
}

/// Parses the top-level override rules from a JSON object.
fn parse_override_rules(
  map: &serde_json::Map<String, serde_json::Value>,
  root_deps: &HashMap<StackString, StackString>,
) -> Result<Vec<NpmOverrideRule>, NpmOverridesError> {
  let mut rules = Vec::with_capacity(map.len());
  for (key, value) in map {
    let (name, selector) = parse_override_key(key)?;
    let (override_value, children) =
      parse_override_value(key, value, root_deps)?;
    rules.push(NpmOverrideRule {
      name,
      selector,
      value: override_value,
      children,
    });
  }
  Ok(rules)
}

#[cfg(test)]
mod test {
  use super::*;

  fn empty_root_deps() -> HashMap<StackString, StackString> {
    HashMap::new()
  }

  #[test]
  fn parse_simple_override() {
    let raw = serde_json::json!({
      "foo": "1.0.0"
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    assert_eq!(overrides.rules.len(), 1);
    let rule = &overrides.rules[0];
    assert_eq!(rule.name.as_str(), "foo");
    assert!(rule.selector.is_none());
    assert!(rule.children.is_empty());
    match &rule.value {
      NpmOverrideValue::Version(req) => {
        assert_eq!(req.version_text(), "1.0.0");
      }
      _ => panic!("expected Version"),
    }
  }

  #[test]
  fn parse_override_with_version_selector() {
    let raw = serde_json::json!({
      "foo@^2.0.0": "2.1.0"
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    assert_eq!(overrides.rules.len(), 1);
    let rule = &overrides.rules[0];
    assert_eq!(rule.name.as_str(), "foo");
    assert!(rule.selector.is_some());
    assert_eq!(rule.selector.as_ref().unwrap().version_text(), "^2.0.0");
    match &rule.value {
      NpmOverrideValue::Version(req) => {
        assert_eq!(req.version_text(), "2.1.0");
      }
      _ => panic!("expected Version"),
    }
  }

  #[test]
  fn parse_scoped_package_override() {
    let raw = serde_json::json!({
      "@scope/pkg": "3.0.0"
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    assert_eq!(overrides.rules.len(), 1);
    let rule = &overrides.rules[0];
    assert_eq!(rule.name.as_str(), "@scope/pkg");
    assert!(rule.selector.is_none());
  }

  #[test]
  fn parse_scoped_package_with_selector() {
    let raw = serde_json::json!({
      "@scope/pkg@^1.0.0": "1.2.3"
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    assert_eq!(overrides.rules.len(), 1);
    let rule = &overrides.rules[0];
    assert_eq!(rule.name.as_str(), "@scope/pkg");
    assert_eq!(rule.selector.as_ref().unwrap().version_text(), "^1.0.0");
  }

  #[test]
  fn parse_nested_override() {
    let raw = serde_json::json!({
      "foo": {
        "bar": "2.0.0"
      }
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    assert_eq!(overrides.rules.len(), 1);
    let rule = &overrides.rules[0];
    assert_eq!(rule.name.as_str(), "foo");
    assert!(matches!(rule.value, NpmOverrideValue::Inherited));
    assert_eq!(rule.children.len(), 1);
    assert_eq!(rule.children[0].name.as_str(), "bar");
    match &rule.children[0].value {
      NpmOverrideValue::Version(req) => {
        assert_eq!(req.version_text(), "2.0.0");
      }
      _ => panic!("expected Version"),
    }
  }

  #[test]
  fn parse_nested_with_dot_key() {
    let raw = serde_json::json!({
      "foo@2.x": {
        ".": "2.1.0",
        "bar": "3.0.0"
      }
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    assert_eq!(overrides.rules.len(), 1);
    let rule = &overrides.rules[0];
    assert_eq!(rule.name.as_str(), "foo");
    assert!(rule.selector.is_some());
    match &rule.value {
      NpmOverrideValue::Version(req) => {
        assert_eq!(req.version_text(), "2.1.0");
      }
      _ => panic!("expected Version from dot key"),
    }
    assert_eq!(rule.children.len(), 1);
    assert_eq!(rule.children[0].name.as_str(), "bar");
  }

  #[test]
  fn parse_dollar_reference() {
    let mut root_deps = HashMap::new();
    root_deps.insert(StackString::from("foo"), StackString::from("^1.0.0"));
    let raw = serde_json::json!({
      "bar": "$foo"
    });
    let overrides = NpmOverrides::from_value(raw, &root_deps).unwrap();
    assert_eq!(overrides.rules.len(), 1);
    let rule = &overrides.rules[0];
    assert_eq!(rule.name.as_str(), "bar");
    match &rule.value {
      NpmOverrideValue::Version(req) => {
        assert_eq!(req.version_text(), "^1.0.0");
      }
      _ => panic!("expected Version from dollar reference"),
    }
  }

  #[test]
  fn parse_dollar_reference_unresolved() {
    let raw = serde_json::json!({
      "bar": "$nonexistent"
    });
    let result = NpmOverrides::from_value(raw, &empty_root_deps());
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("nonexistent"));
  }

  #[test]
  fn parse_empty_overrides() {
    let raw = serde_json::json!({});
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    assert!(overrides.is_empty());
  }

  #[test]
  fn parse_null_overrides() {
    let raw = serde_json::Value::Null;
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    assert!(overrides.is_empty());
  }

  #[test]
  fn parse_empty_string_override() {
    let raw = serde_json::json!({
      "foo": ""
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    assert_eq!(overrides.rules.len(), 1);
    assert!(matches!(
      overrides.rules[0].value,
      NpmOverrideValue::Removed
    ));
  }

  #[test]
  fn parse_empty_string_dot_key() {
    let raw = serde_json::json!({
      "foo": {
        ".": "",
        "bar": "2.0.0"
      }
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    assert_eq!(overrides.rules.len(), 1);
    assert!(matches!(
      overrides.rules[0].value,
      NpmOverrideValue::Removed
    ));
    assert_eq!(overrides.rules[0].children.len(), 1);
  }

  #[test]
  fn parse_invalid_dot_value_type() {
    let raw = serde_json::json!({
      "foo": {
        ".": 42,
        "bar": "2.0.0"
      }
    });
    let result = NpmOverrides::from_value(raw, &empty_root_deps());
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("\".\""));
    assert!(err.contains("expected a string"));
  }

  #[test]
  fn active_overrides_empty() {
    let overrides = NpmOverrides::default();
    let active = ActiveOverrides::from_overrides(&overrides);
    assert!(active.is_empty());
    assert!(
      active
        .get_override_for(&PackageName::from_str("foo"), None)
        .is_none()
    );
  }

  #[test]
  fn active_overrides_simple_global() {
    let raw = serde_json::json!({
      "foo": "1.0.0"
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    let active = ActiveOverrides::from_overrides(&overrides);

    // should find override for "foo" (no selector, so None version works)
    let result = active.get_override_for(&PackageName::from_str("foo"), None);
    assert!(result.is_some());
    assert_eq!(result.unwrap().version_text(), "1.0.0");

    // should not find override for "bar"
    assert!(
      active
        .get_override_for(&PackageName::from_str("bar"), None)
        .is_none()
    );
  }

  #[test]
  fn active_overrides_for_child_passthrough() {
    // global override should pass through to child contexts (for non-matching packages)
    let raw = serde_json::json!({
      "foo": "1.0.0"
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    let active = ActiveOverrides::from_overrides(&overrides);

    // descend into "bar@2.0.0" — "foo" override should still be active
    let child_active = active.for_child(
      &PackageName::from_str("bar"),
      &Version::parse_from_npm("2.0.0").unwrap(),
    );
    let result =
      child_active.get_override_for(&PackageName::from_str("foo"), None);
    assert!(result.is_some());
    assert_eq!(result.unwrap().version_text(), "1.0.0");
  }

  #[test]
  fn active_overrides_for_child_consumed() {
    // a simple override for "foo" should be consumed when descending into "foo"
    let raw = serde_json::json!({
      "foo": "1.0.0"
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    let active = ActiveOverrides::from_overrides(&overrides);

    // descend into "foo@1.0.0" — the override is consumed (it targeted this node)
    let child_active = active.for_child(
      &PackageName::from_str("foo"),
      &Version::parse_from_npm("1.0.0").unwrap(),
    );
    // "foo" override should no longer be active for deeper descendants
    assert!(
      child_active
        .get_override_for(&PackageName::from_str("foo"), None)
        .is_none()
    );
  }

  #[test]
  fn active_overrides_scoped_override() {
    // scoped override: "parent": { "child": "2.0.0" }
    let raw = serde_json::json!({
      "parent": {
        "child": "2.0.0"
      }
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    let active = ActiveOverrides::from_overrides(&overrides);

    // at the top level, there's no override for "child"
    assert!(
      active
        .get_override_for(&PackageName::from_str("child"), None)
        .is_none()
    );

    // descend into "parent@1.0.0" — child overrides should become active
    let child_active = active.for_child(
      &PackageName::from_str("parent"),
      &Version::parse_from_npm("1.0.0").unwrap(),
    );
    let result =
      child_active.get_override_for(&PackageName::from_str("child"), None);
    assert!(result.is_some());
    assert_eq!(result.unwrap().version_text(), "2.0.0");

    // descend into "other@1.0.0" — "child" override should NOT be active
    let other_active = active.for_child(
      &PackageName::from_str("other"),
      &Version::parse_from_npm("1.0.0").unwrap(),
    );
    assert!(
      other_active
        .get_override_for(&PackageName::from_str("child"), None)
        .is_none()
    );
  }

  #[test]
  fn active_overrides_selector_match() {
    // override with version selector: "foo@^2.0.0": { "bar": "3.0.0" }
    let raw = serde_json::json!({
      "foo@^2.0.0": {
        "bar": "3.0.0"
      }
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    let active = ActiveOverrides::from_overrides(&overrides);

    // descend into "foo@2.1.0" (matches ^2.0.0) — bar override should be active
    let matching = active.for_child(
      &PackageName::from_str("foo"),
      &Version::parse_from_npm("2.1.0").unwrap(),
    );
    let result = matching.get_override_for(&PackageName::from_str("bar"), None);
    assert!(result.is_some());
    assert_eq!(result.unwrap().version_text(), "3.0.0");

    // descend into "foo@1.0.0" (does NOT match ^2.0.0) — bar override should NOT be active
    let non_matching = active.for_child(
      &PackageName::from_str("foo"),
      &Version::parse_from_npm("1.0.0").unwrap(),
    );
    assert!(
      non_matching
        .get_override_for(&PackageName::from_str("bar"), None)
        .is_none()
    );
  }

  #[test]
  fn active_overrides_dot_key_with_selector() {
    // override with "." key and selector:
    // "foo@^2.0.0": { ".": "2.1.0", "bar": "3.0.0" }
    let raw = serde_json::json!({
      "foo@^2.0.0": {
        ".": "2.1.0",
        "bar": "3.0.0"
      }
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    let active = ActiveOverrides::from_overrides(&overrides);

    // without a resolved version, the selector-based override is not returned
    assert!(
      active
        .get_override_for(&PackageName::from_str("foo"), None)
        .is_none()
    );

    // with a matching version, the "." override is returned
    let result = active.get_override_for(
      &PackageName::from_str("foo"),
      Some(&Version::parse_from_npm("2.5.0").unwrap()),
    );
    assert!(result.is_some());
    assert_eq!(result.unwrap().version_text(), "2.1.0");

    // with a non-matching version, the override is not returned
    assert!(
      active
        .get_override_for(
          &PackageName::from_str("foo"),
          Some(&Version::parse_from_npm("1.0.0").unwrap()),
        )
        .is_none()
    );
  }

  #[test]
  fn active_overrides_selector_on_direct_value() {
    // "foo@^2.0.0": "2.1.0" — selector on a direct version override
    let raw = serde_json::json!({
      "foo@^2.0.0": "2.1.0"
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    let active = ActiveOverrides::from_overrides(&overrides);

    // without version: not returned (has selector)
    assert!(
      active
        .get_override_for(&PackageName::from_str("foo"), None)
        .is_none()
    );

    // matching version: returned
    let result = active.get_override_for(
      &PackageName::from_str("foo"),
      Some(&Version::parse_from_npm("2.3.0").unwrap()),
    );
    assert!(result.is_some());
    assert_eq!(result.unwrap().version_text(), "2.1.0");

    // non-matching version: not returned
    assert!(
      active
        .get_override_for(
          &PackageName::from_str("foo"),
          Some(&Version::parse_from_npm("1.5.0").unwrap()),
        )
        .is_none()
    );
  }

  #[test]
  fn active_overrides_removed_cancels_override() {
    // simulate a scoped removal: after entering a scope, the active rules
    // would be [{name: "foo", value: Removed}, {name: "foo", value: Version("1.0.0")}]
    // the Removed should cancel the subsequent Version rule
    let active = ActiveOverrides {
      rules: vec![
        NpmOverrideRule {
          name: PackageName::from_str("foo"),
          selector: None,
          value: NpmOverrideValue::Removed,
          children: Vec::new(),
        },
        NpmOverrideRule {
          name: PackageName::from_str("foo"),
          selector: None,
          value: NpmOverrideValue::Version(
            VersionReq::parse_from_npm("1.0.0").unwrap(),
          ),
          children: Vec::new(),
        },
      ],
    };

    // Removed comes first, so get_override_for returns None
    assert!(
      active
        .get_override_for(&PackageName::from_str("foo"), None)
        .is_none()
    );
  }

  #[test]
  fn for_child_scoped_rules_take_precedence() {
    // "foo": "1.0.0" (global) and "parent": { "foo": "" } (scoped removal)
    // after entering "parent", the scoped removal should take precedence
    let raw = serde_json::json!({
      "foo": "1.0.0",
      "parent": {
        "foo": ""
      }
    });
    let overrides = NpmOverrides::from_value(raw, &empty_root_deps()).unwrap();
    let active = ActiveOverrides::from_overrides(&overrides);

    // at top level, "foo" override is active
    let result = active.get_override_for(&PackageName::from_str("foo"), None);
    assert!(result.is_some());
    assert_eq!(result.unwrap().version_text(), "1.0.0");

    // enter "parent@1.0.0" — the scoped removal should cancel the global override
    let inside_parent = active.for_child(
      &PackageName::from_str("parent"),
      &Version::parse_from_npm("1.0.0").unwrap(),
    );
    assert!(
      inside_parent
        .get_override_for(&PackageName::from_str("foo"), None)
        .is_none()
    );
  }
}
