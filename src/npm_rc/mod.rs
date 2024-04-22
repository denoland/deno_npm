// Copyright 2018-2024 the Deno authors. MIT license.

mod ini;

pub struct NpmRc {
}

pub fn parse_npm_rc(input: &str) -> Result<NpmRc, monch::ParseErrorFailureError> {
  let ini_file = ini::parse_ini(input)?;

  //ini_file


  Ok(NpmRc {})
}