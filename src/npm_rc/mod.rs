// Copyright 2018-2024 the Deno authors. MIT license.

use monch::*;
use std::collections::HashMap;

use self::ini::Key;
use self::ini::KeyValueOrSection;
use self::ini::Value;

mod ini;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct RegistryConfig {
  pub auth: Option<String>,
  pub auth_token: Option<String>,
  pub username: Option<String>,
  pub password: Option<String>,
  pub email: Option<String>,
  pub certfile: Option<String>,
  pub keyfile: Option<String>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NpmRc {
  pub scope_registries: HashMap<String, String>,
  pub registry_configs: HashMap<String, RegistryConfig>,
}

pub fn parse_npm_rc(
  input: &str,
  get_env_var: &impl Fn(&str) -> Option<String>,
) -> Result<NpmRc, monch::ParseErrorFailureError> {
  let kv_or_sections = ini::parse_ini(input)?;
  let mut rc_file = NpmRc::default();

  for kv_or_section in kv_or_sections {
    match kv_or_section {
      KeyValueOrSection::KeyValue(kv) => {
        if let Key::Plain(key) = &kv.key {
          if let Some((left, right)) = key.rsplit_once(':') {
            if let Some(scope) = left.strip_prefix('@') {
              if right == "registry" {
                if let Value::String(text) = &kv.value {
                  let value = expand_vars(text, get_env_var);
                  rc_file.scope_registries.insert(scope.to_string(), value);
                }
              }
            } else if let Some(host_and_path) = left.strip_prefix("//") {
              if let Value::String(text) = &kv.value {
                let value = expand_vars(text, get_env_var);
                let config = rc_file
                  .registry_configs
                  .entry(host_and_path.to_string())
                  .or_default();
                match right {
                  "_auth" => {
                    config.auth = Some(value);
                  }
                  "_authToken" => {
                    config.auth_token = Some(value);
                  }
                  "username" => {
                    config.username = Some(value);
                  }
                  "_password" => {
                    config.password = Some(value);
                  }
                  "email" => {
                    config.email = Some(value);
                  }
                  "certfile" => {
                    config.certfile = Some(value);
                  }
                  "keyfile" => {
                    config.keyfile = Some(value);
                  }
                  _ => {}
                }
              }
            }
          }
        }
      }
      KeyValueOrSection::Section(_) => {
        // ignore
      }
    }
  }

  Ok(rc_file)
}

fn expand_vars(
  input: &str,
  get_env_var: &impl Fn(&str) -> Option<String>,
) -> String {
  fn escaped_char(input: &str) -> ParseResult<char> {
    preceded(ch('\\'), next_char)(input)
  }

  fn env_var(input: &str) -> ParseResult<&str> {
    let (input, _) = tag("${")(input)?;
    let (input, var_name) = take_while(|c| c != '}')(input)?;
    if var_name.chars().any(|c| matches!(c, '$' | '{' | '\\')) {
      return ParseError::backtrace();
    }
    let (input, _) = ch('}')(input)?;
    Ok((input, var_name))
  }

  let (input, results) = many0(or3(
    map(escaped_char, |c| c.to_string()),
    map(env_var, |var_name| {
      if let Some(var_value) = get_env_var(var_name) {
        var_value
      } else {
        format!("${{{}}}", var_name)
      }
    }),
    map(next_char, |c| c.to_string()),
  ))(input)
  .unwrap();
  assert!(input.is_empty());
  results.join("")
}

#[cfg(test)]
mod test {
  use super::*;

  use pretty_assertions::assert_eq;

  #[test]
  fn test_parse_basic() {
    // https://docs.npmjs.com/cli/v10/configuring-npm/npmrc#auth-related-configuration
    let npm_rc = parse_npm_rc(
      r#"
@myorg:registry=https://example.com/myorg
@another:registry=https://example.com/another
//registry.npmjs.org/:_authToken=MYTOKEN
; would apply to both @myorg and @another
//example.com/:_authToken=MYTOKEN
//example.com/:_auth=AUTH
//example.com/:username=USERNAME
//example.com/:_password=PASSWORD
//example.com/:email=EMAIL
//example.com/:certfile=CERTFILE
//example.com/:keyfile=KEYFILE
; would apply only to @myorg
//example.com/myorg/:_authToken=MYTOKEN1
; would apply only to @another
//example.com/another/:_authToken=MYTOKEN2
"#,
      &|_| None,
    )
    .unwrap();
    assert_eq!(
      npm_rc,
      NpmRc {
        scope_registries: HashMap::from([
          ("myorg".to_string(), "https://example.com/myorg".to_string()),
          (
            "another".to_string(),
            "https://example.com/another".to_string()
          )
        ]),
        registry_configs: HashMap::from([
          (
            "example.com/".to_string(),
            RegistryConfig {
              auth: Some("AUTH".to_string()),
              auth_token: Some("MYTOKEN".to_string()),
              username: Some("USERNAME".to_string()),
              password: Some("PASSWORD".to_string()),
              email: Some("EMAIL".to_string()),
              certfile: Some("CERTFILE".to_string()),
              keyfile: Some("KEYFILE".to_string()),
            }
          ),
          (
            "example.com/another/".to_string(),
            RegistryConfig {
              auth_token: Some("MYTOKEN2".to_string()),
              ..Default::default()
            }
          ),
          (
            "example.com/myorg/".to_string(),
            RegistryConfig {
              auth_token: Some("MYTOKEN1".to_string()),
              ..Default::default()
            }
          ),
          (
            "registry.npmjs.org/".to_string(),
            RegistryConfig {
              auth_token: Some("MYTOKEN".to_string()),
              ..Default::default()
            }
          ),
        ])
      }
    )
  }

  #[test]
  fn test_parse_env_vars() {
    let npm_rc = parse_npm_rc(
      r#"
@myorg:registry=${VAR_FOUND}
@another:registry=${VAR_NOT_FOUND}
@a:registry=\${VAR_FOUND}
//registry.npmjs.org/:_authToken=${VAR_FOUND}
"#,
      &|var_name| match var_name {
        "VAR_FOUND" => Some("SOME_VALUE".to_string()),
        _ => None,
      },
    )
    .unwrap();
    assert_eq!(
      npm_rc,
      NpmRc {
        scope_registries: HashMap::from([
          ("a".to_string(), "${VAR_FOUND}".to_string()),
          ("myorg".to_string(), "SOME_VALUE".to_string()),
          ("another".to_string(), "${VAR_NOT_FOUND}".to_string()),
        ]),
        registry_configs: HashMap::from([(
          "registry.npmjs.org/".to_string(),
          RegistryConfig {
            auth_token: Some("SOME_VALUE".to_string()),
            ..Default::default()
          }
        ),])
      }
    )
  }

  #[test]
  fn test_expand_vars() {
    assert_eq!(
      expand_vars("test${VAR}test", &|var_name| {
        match var_name {
          "VAR" => Some("VALUE".to_string()),
          _ => None,
        }
      }),
      "testVALUEtest"
    );
    assert_eq!(
      expand_vars("${A}${B}${C}", &|var_name| {
        match var_name {
          "A" => Some("1".to_string()),
          "B" => Some("2".to_string()),
          "C" => Some("3".to_string()),
          _ => None,
        }
      }),
      "123"
    );
    assert_eq!(
      expand_vars("test\\${VAR}test", &|var_name| {
        match var_name {
          "VAR" => Some("VALUE".to_string()),
          _ => None,
        }
      }),
      "test${VAR}test"
    );
    assert_eq!(
      // npm ignores values with $ in them
      expand_vars("test${VA$R}test", &|_| {
        unreachable!();
      }),
      "test${VA$R}test"
    );
    assert_eq!(
      // npm ignores values with { in them
      expand_vars("test${VA{R}test", &|_| {
        unreachable!();
      }),
      "test${VA{R}test"
    );
  }
}
