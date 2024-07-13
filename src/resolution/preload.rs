// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::HashMap;

use deno_semver::package::PackageNv;

use crate::registry::NpmPackageVersionInfo;
use crate::registry::NpmRegistryApi;
use crate::NpmSystemInfo;

enum PreloadStatus {
  PendingOptional,
  PendingRequired,
  OptionalNotMatchedSystem(Box<NpmPackageVersionInfo>),
  Seen,
}

pub struct PreloadContext<'a, TNpmRegistryApi: NpmRegistryApi> {
  api: &'a TNpmRegistryApi,
  system_info: NpmSystemInfo,
  pkgs: HashMap<PackageNv, PreloadStatus>,
}

impl<'a, TNpmRegistryApi: NpmRegistryApi> PreloadContext<'a, TNpmRegistryApi> {
  pub fn new(
    // todo: object bag
    api: &'a TNpmRegistryApi,
    system_info: NpmSystemInfo,
    maybe_capacity: Option<usize>,
    root_pkgs: impl Iterator<Item = PackageNv>,
  ) -> Self {
    let mut ctx = Self {
      api,
      system_info,
      pkgs: maybe_capacity
        .map(|capacity| HashMap::with_capacity(capacity))
        .unwrap_or_default(),
    };
    ctx
      .pkgs
      .extend(root_pkgs.map(|nv| (nv, PreloadStatus::PendingRequired)));
    ctx
  }

  pub fn handle_package<'b>(
    &mut self,
    nv: &PackageNv,
    version_info: &NpmPackageVersionInfo,
    deps: impl Iterator<Item = (&'b str, &'b PackageNv)>,
  ) {
    let should_call = match self.pkgs.get(nv) {
      Some(PreloadStatus::PendingRequired) => {
        self.pkgs.insert(nv.clone(), PreloadStatus::Seen);
        true
      }
      Some(PreloadStatus::PendingOptional) => {
        self.pkgs.insert(nv.clone(), PreloadStatus::Seen);
        true
      }
      Some(PreloadStatus::OptionalNotMatchedSystem(_)) => false,
      Some(PreloadStatus::Seen) => false,
      None => {
        if version_info.matches_system(&self.system_info) {
          self.pkgs.insert(nv.clone(), PreloadStatus::Seen);
          true
        } else {
          self.pkgs.insert(
            nv.clone(),
            PreloadStatus::OptionalNotMatchedSystem(Box::new(
              version_info.clone(),
            )),
          );
          false
        }
      }
    };
    if should_call {
      self.api.preload_package_nv(nv, version_info);
    }

    for (key, nv) in deps {
      if version_info.optional_dependencies.contains_key(key) {
        self.handle_optional_dep(nv);
      } else {
        self.handle_required_dep(nv);
      }
    }
  }

  fn handle_required_dep(&mut self, nv: &PackageNv) {
    if let Some(status) = self.pkgs.get_mut(&nv) {
      match status {
        PreloadStatus::PendingOptional => {
          *status = PreloadStatus::PendingRequired
        }
        PreloadStatus::OptionalNotMatchedSystem(version_info) => {
          self.api.preload_package_nv(nv, version_info);
          *status = PreloadStatus::Seen;
        }
        PreloadStatus::PendingRequired => {}
        PreloadStatus::Seen => {}
      }
    } else {
      self.pkgs.insert(nv.clone(), PreloadStatus::PendingRequired);
    }
  }

  pub fn handle_optional_dep(&mut self, nv: &PackageNv) {
    if let Some(status) = self.pkgs.get_mut(&nv) {
      match status {
        PreloadStatus::PendingRequired => {
          *status = PreloadStatus::PendingOptional
        }
        PreloadStatus::OptionalNotMatchedSystem(_) => {
          // seen
          *status = PreloadStatus::PendingOptional;
        }
        PreloadStatus::PendingOptional => {}
        PreloadStatus::Seen => {}
      }
    } else {
      self.pkgs.insert(nv.clone(), PreloadStatus::PendingOptional);
    }
  }
}
