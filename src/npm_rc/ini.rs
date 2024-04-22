// Copyright 2018-2024 the Deno authors. MIT license.

// ini file parsing

use monch::*;

#[derive(Debug, PartialEq, Eq)]
pub enum KeyValueOrSection<'a> {
  KeyValue(KeyValue<'a>),
  Section(Section<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Section<'a> {
  header: &'a str,
  items: Vec<KeyValue<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct KeyValue<'a> {
  key: Key<'a>,
  value: Value<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Key<'a> {
  Plain(&'a str),
  Array(&'a str),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Value<'a> {
  String(&'a str),
  Boolean(bool),
  Number(i64),
  Null,
  Undefined,
}

pub fn parse_ini(
  input: &str,
) -> Result<Vec<KeyValueOrSection>, ParseErrorFailureError> {
  with_failure_handling(|input| {
    let (input, _) = skip_trivia(input)?;
    let (input, items) = many0(|input| {
      let (input, kv_or_section) = parse_kv_or_section(input)?;
      let (input, _) = skip_trivia(input)?;
      Ok((input, kv_or_section))
    })(input)?;
    Ok((input, items))
  })(input)
}

fn parse_kv_or_section(input: &str) -> ParseResult<KeyValueOrSection> {
  or(
    map(parse_key_value, KeyValueOrSection::KeyValue),
    map(parse_section, KeyValueOrSection::Section),
  )(input)
}

fn parse_section(input: &str) -> ParseResult<Section> {
  let (input, _) = skip_non_newline_whitespace(input)?;
  let (input, header) = parse_section_header(input)?;
  let (input, _) = skip_non_newline_whitespace(input)?;
  let (input, _) = skip_trivia(input)?;
  let (input, items) = many0(|input| {
    let (input, kv) = parse_key_value(input)?;
    let (input, _) = skip_trivia(input)?;
    Ok((input, kv))
  })(input)?;
  Ok((input, Section { header, items }))
}

fn parse_section_header(input: &str) -> ParseResult<&str> {
  let (input, _) = ch('[')(input)?;
  let (input, header_text) = take_while(|c| c != ']' && c != '\n')(input)?;
  let (input, _) = ch(']')(input)?;

  Ok((input, header_text))
}

fn parse_key_value(input: &str) -> ParseResult<KeyValue> {
  fn parse_empty_value(input: &str) -> ParseResult<()> {
    let (input, _) = skip_non_newline_whitespace(input)?;
    let (input, _) = skip_comment(input)?;
    if input.is_empty() || input.starts_with('\n') || input.starts_with("\r\n")
    {
      Ok((input, ()))
    } else {
      Err(ParseError::Backtrace)
    }
  }

  let (input, key) = parse_key(input)?;
  let (input, _) = skip_non_newline_whitespace(input)?;
  let (input, value) = or(
    |input| {
      let (input, _) = ch('=')(input)?;
      parse_value(input)
    },
    map(parse_empty_value, |_| Value::Boolean(true)),
  )(input)?;
  Ok((input, KeyValue { key, value }))
}

fn parse_key(input: &str) -> ParseResult<Key> {
  fn parse_unquoted(input: &str) -> ParseResult<Key> {
    let (input, key) =
      take_while_not_comment_and(|c| c != '=' && c != '\n')(input)?;
    let key = key.trim();
    match key.strip_suffix("[]") {
      Some(key) => Ok((input, Key::Array(key))),
      None => Ok((input, Key::Plain(key))),
    }
  }

  or(
    map(parse_quoted_skipping_spaces, Key::Plain),
    parse_unquoted,
  )(input)
}

fn parse_value(input: &str) -> ParseResult<Value> {
  fn parse_unquoted(input: &str) -> ParseResult<Value> {
    let (input, value) = take_until_comment_or_newline(input)?;
    let value = value.trim();
    Ok((
      input,
      match value {
        "true" => Value::Boolean(true),
        "false" => Value::Boolean(false),
        "null" => Value::Null,
        "undefined" => Value::Undefined,
        value => {
          if let Ok(value) = value.parse::<i64>() {
            Value::Number(value)
          } else {
            Value::String(value)
          }
        },
      },
    ))
  }

  or(
    map(parse_quoted_skipping_spaces, Value::String),
    parse_unquoted,
  )(input)
}

fn skip_trivia(input: &str) -> ParseResult<()> {
  let mut input = input;
  let mut length = 0;

  while input.len() != length {
    length = input.len();
    input = skip_whitespace(input)?.0;
    input = skip_comment(input)?.0;
  }
  Ok((input, ()))
}

fn parse_quoted_skipping_spaces(input: &str) -> ParseResult<&str> {
  let (input, _) = skip_non_newline_whitespace(input)?;
  let (input, value) = parse_quoted_string(input)?;
  let (input, _) = skip_non_newline_whitespace(input)?;
  Ok((input, value))
}

fn parse_quoted_string(input: &str) -> ParseResult<&str> {
  let (input, quote_start_char) = or(ch('"'), ch('\''))(input)?;
  let (input, quoted_text) =
    take_while(|c| c != quote_start_char && c != '\n')(input)?;
  let (input, _) = ch(quote_start_char)(input)?;
  Ok((input, quoted_text))
}

fn skip_non_newline_whitespace(input: &str) -> ParseResult<()> {
  skip_while(|c| c == ' ' || c == '\t')(input)
}

fn skip_comment(input: &str) -> ParseResult<()> {
  let (input, maybe_found) =
    maybe(or(map(ch('#'), |_| ()), map(ch(';'), |_| ())))(input)?;
  if maybe_found.is_none() {
    return Ok((input, ()));
  }
  let (input, _) = skip_while(|c| c != '\n')(input)?;

  Ok((input, ()))
}

fn take_until_comment_or_newline(input: &str) -> ParseResult<&str> {
  take_while_not_comment_and(|c| c != '\n')(input)
}

fn take_while_not_comment_and<'a>(
  test: impl Fn(char) -> bool,
) -> impl Fn(&'a str) -> ParseResult<'a, &'a str> {
  move |input| {
    let mut last_char = None;
    for (index, c) in input.char_indices() {
      if matches!(c, '#' | ';') && last_char != Some('\\') || !test(c) {
        return Ok((&input[index..], &input[..index]));
      }
      last_char = Some(c);
    }
    Ok(("", input))
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn parses_ini() {
    let ini = parse_ini(
      r#"
a=1
b="2"
c    =    '3'
d
e = true;comment
f = false # comment;test#;comment
g = null
h = undefined
i[] = 1
i[] = 2
"#,
    )
    .unwrap();
    assert_eq!(
      ini,
      vec![
        KeyValueOrSection::KeyValue(KeyValue {
          key: Key::Plain("a"),
          value: Value::Number(1),
        }),
        KeyValueOrSection::KeyValue(KeyValue {
          key: Key::Plain("b"),
          value: Value::String("2"),
        }),
        KeyValueOrSection::KeyValue(KeyValue {
          key: Key::Plain("c"),
          value: Value::String("3"),
        }),
        KeyValueOrSection::KeyValue(KeyValue {
          key: Key::Plain("d"),
          value: Value::Boolean(true)
        }),
        KeyValueOrSection::KeyValue(KeyValue {
          key: Key::Plain("e"),
          value: Value::Boolean(true),
        }),
        KeyValueOrSection::KeyValue(KeyValue {
          key: Key::Plain("f"),
          value: Value::Boolean(false),
        }),
        KeyValueOrSection::KeyValue(KeyValue {
          key: Key::Plain("g"),
          value: Value::Null,
        }),
        KeyValueOrSection::KeyValue(KeyValue {
          key: Key::Plain("h"),
          value: Value::Undefined,
        }),
        KeyValueOrSection::KeyValue(KeyValue {
          key: Key::Array("i"),
          value: Value::Number(1),
        }),
        KeyValueOrSection::KeyValue(KeyValue {
          key: Key::Array("i"),
          value: Value::Number(2),
        }),
      ]
    )
  }
}
