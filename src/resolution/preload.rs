// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use deno_semver::package::PackageNv;

use crate::registry::NpmPackageVersionInfo;
use crate::registry::NpmRegistryApi;
use crate::NpmSystemInfo;

enum PreloadStatus {
  PendingOptional,
  PendingRequired,
  PendingOptionalSeen(Arc<NpmPackageVersionInfo>),
  UnknownSeen(Arc<NpmPackageVersionInfo>),
  Handled,
}

pub struct PreloadOptions {
  pub system_info: NpmSystemInfo,
  pub maybe_capacity: Option<usize>,
}

pub struct PreloadContext<'a, TNpmRegistryApi: NpmRegistryApi> {
  api: &'a TNpmRegistryApi,
  system_info: NpmSystemInfo,
  pkgs: HashMap<Rc<PackageNv>, PreloadStatus>,
}

impl<'a, TNpmRegistryApi: NpmRegistryApi> PreloadContext<'a, TNpmRegistryApi> {
  pub fn new(api: &'a TNpmRegistryApi, options: PreloadOptions) -> Self {
    Self {
      api,
      system_info: options.system_info,
      pkgs: options
        .maybe_capacity
        .map(HashMap::with_capacity)
        .unwrap_or_default(),
    }
  }

  pub fn handle_package(
    &mut self,
    nv: &Rc<PackageNv>,
    version_info: &Arc<NpmPackageVersionInfo>,
  ) {
    match self.pkgs.get_mut(nv) {
      Some(status) => match status {
        PreloadStatus::PendingRequired => {
          *status = PreloadStatus::Handled;
          self.api.preload_package_nv(nv, version_info);
        }
        PreloadStatus::PendingOptional => {
          if version_info.matches_system(&self.system_info) {
            *status = PreloadStatus::Handled;
            self.api.preload_package_nv(nv, version_info);
          } else {
            // it might be marked as required later
            *status = PreloadStatus::PendingOptionalSeen(version_info.clone());
          }
        }
        PreloadStatus::PendingOptionalSeen(_)
        | PreloadStatus::UnknownSeen(_)
        | PreloadStatus::Handled => {}
      },
      None => {
        self
          .pkgs
          .insert(nv.clone(), PreloadStatus::UnknownSeen(version_info.clone()));
      }
    }
  }

  pub fn system_info(&self) -> &NpmSystemInfo {
    &self.system_info
  }

  pub fn mark_required_dep(&mut self, nv: &Rc<PackageNv>) {
    if let Some(status) = self.pkgs.get_mut(nv) {
      match status {
        PreloadStatus::UnknownSeen(info)
        | PreloadStatus::PendingOptionalSeen(info) => {
          self.api.preload_package_nv(nv, info);
          *status = PreloadStatus::Handled
        }
        PreloadStatus::PendingRequired => {}
        PreloadStatus::PendingOptional => {
          *status = PreloadStatus::PendingRequired;
        }
        PreloadStatus::Handled => {}
      }
    } else {
      self.pkgs.insert(nv.clone(), PreloadStatus::PendingRequired);
    }
  }

  pub fn mark_optional_dep(&mut self, nv: &Rc<PackageNv>) {
    if !self.pkgs.contains_key(nv) {
      self.pkgs.insert(nv.clone(), PreloadStatus::PendingOptional);
    }
  }
}
