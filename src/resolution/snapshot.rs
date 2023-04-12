// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::hash_map;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use deno_semver::npm::NpmPackageNv;
use deno_semver::npm::NpmPackageReq;
use deno_semver::VersionReq;
use futures::future::Either;
use futures::StreamExt;
use log::debug;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

use super::common::NpmVersionResolver;
use super::common::LATEST_VERSION_REQ;
use super::graph::Graph;
use super::graph::GraphDependencyResolver;
use super::graph::NpmResolutionError;
use super::NpmPackageVersionResolutionError;

use crate::registry::NpmPackageInfo;
use crate::registry::NpmPackageVersionDistInfo;
use crate::registry::NpmRegistryApi;
use crate::registry::NpmRegistryPackageInfoLoadError;
use crate::NpmPackageCacheFolderId;
use crate::NpmPackageId;
use crate::NpmResolutionPackage;

#[derive(Debug, Error, Clone)]
#[error("Could not find referenced package '{}' in the list of packages.", self.0.as_serialized())]
pub struct PackageIdNotFoundError(pub NpmPackageId);

#[derive(Debug, Error, Clone)]
#[error(
  "Could not find referenced package constraint '{0}' in the list of packages."
)]
pub struct PackageReqNotFoundError(pub NpmPackageReq);

#[derive(Debug, Error, Clone)]
#[error("Could not find referenced package '{0}' in the list of packages.")]
pub struct PackageNvNotFoundError(pub NpmPackageNv);

#[derive(Debug, Error, Clone)]
pub enum PackageNotFoundFromReferrerError {
  #[error("Could not find referrer npm package '{0}'.")]
  Referrer(NpmPackageCacheFolderId),
  #[error("Could not find npm package '{name}' referenced by '{referrer}'.")]
  Package {
    name: String,
    referrer: NpmPackageCacheFolderId,
  },
}

/// Packages partitioned by if they are "copy" packages or not.
pub struct NpmPackagesPartitioned {
  pub packages: Vec<NpmResolutionPackage>,
  /// Since peer dependency resolution occurs based on ancestors and ancestor
  /// siblings, this may sometimes cause the same package (name and version)
  /// to have different dependencies based on where it appears in the tree.
  /// For these packages, we create a "copy package" or duplicate of the package
  /// whose dependencies are that of where in the tree they've resolved to.
  pub copy_packages: Vec<NpmResolutionPackage>,
}

impl NpmPackagesPartitioned {
  pub fn into_all(self) -> Vec<NpmResolutionPackage> {
    let mut packages = self.packages;
    packages.extend(self.copy_packages);
    packages
  }
}

/// A serialized snapshot that has been verified to be non-corrupt
/// and valid.
#[derive(Default)]
pub struct ValidSerializedNpmResolutionSnapshot(
  // keep private -- once verified the caller
  // shouldn't be able to modify it
  SerializedNpmResolutionSnapshot,
);

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SerializedNpmResolutionSnapshotPackage {
  pub pkg_id: NpmPackageId,
  pub dist: NpmPackageVersionDistInfo,
  /// Key is what the package refers to the other package as,
  /// which could be different from the package name.
  pub dependencies: HashMap<String, NpmPackageId>,
}

#[derive(Default, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SerializedNpmResolutionSnapshot {
  /// Resolved npm specifiers to package id mappings.
  pub root_packages: HashMap<NpmPackageReq, NpmPackageId>,
  /// Collection of resolved packages in the dependency graph.
  pub packages: Vec<SerializedNpmResolutionSnapshotPackage>,
}

impl SerializedNpmResolutionSnapshot {
  /// Marks the serialized snapshot as valid, if able.
  ///
  /// Snapshots from serialized sources might be invalid due to tampering
  /// by the user. For example, this could be populated from a lockfile
  /// that the user modified.
  pub fn into_valid(
    self,
  ) -> Result<ValidSerializedNpmResolutionSnapshot, PackageIdNotFoundError> {
    let mut verify_ids = HashSet::with_capacity(self.packages.len());

    // collect the specifiers to version mappings
    verify_ids.extend(self.root_packages.values());

    // then the packages
    let mut package_ids = HashSet::with_capacity(self.packages.len());
    for package in &self.packages {
      package_ids.insert(&package.pkg_id);
      verify_ids.extend(package.dependencies.values());
    }

    // verify that all these ids exist in packages
    for id in verify_ids {
      if !package_ids.contains(&id) {
        return Err(PackageIdNotFoundError(id.clone()));
      }
    }

    Ok(ValidSerializedNpmResolutionSnapshot(self))
  }
}

impl std::fmt::Debug for SerializedNpmResolutionSnapshot {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    // do a custom debug implementation that creates deterministic output for the tests
    f.debug_struct("SerializedNpmResolutionSnapshot")
      .field(
        "root_packages",
        &self.root_packages.iter().collect::<BTreeMap<_, _>>(),
      )
      .field("packages", &self.packages)
      .finish()
  }
}

pub struct NpmResolutionSnapshotCreateOptions {
  pub api: Arc<dyn NpmRegistryApi>,
  pub snapshot: ValidSerializedNpmResolutionSnapshot,
  /// Known good version requirement to use for the `@types/node` package
  /// when the version is unspecified or "latest".
  pub types_node_version_req: Option<VersionReq>,
}

#[derive(Clone)]
pub struct NpmResolutionSnapshot {
  pub(super) api: Arc<dyn NpmRegistryApi>,
  pub(super) version_resolver: NpmVersionResolver,
  /// The unique package requirements map to a single npm package name and version.
  pub(super) package_reqs: HashMap<NpmPackageReq, NpmPackageNv>,
  // Each root level npm package name and version maps to an exact npm package node id.
  pub(super) root_packages: HashMap<NpmPackageNv, NpmPackageId>,
  pub(super) packages_by_name: HashMap<String, Vec<NpmPackageId>>,
  pub(super) packages: HashMap<NpmPackageId, NpmResolutionPackage>,
  /// Ordered list based on resolution of packages whose dependencies
  /// have not yet been resolved
  pub(super) pending_unresolved_packages: Vec<NpmPackageNv>,
}

impl NpmResolutionSnapshot {
  pub fn new(options: NpmResolutionSnapshotCreateOptions) -> Self {
    let snapshot = options.snapshot.0;
    let mut package_reqs =
      HashMap::<NpmPackageReq, NpmPackageNv>::with_capacity(
        snapshot.root_packages.len(),
      );
    let mut root_packages =
      HashMap::<NpmPackageNv, NpmPackageId>::with_capacity(
        snapshot.root_packages.len(),
      );
    let mut packages_by_name =
      HashMap::<String, Vec<NpmPackageId>>::with_capacity(
        snapshot.packages.len(),
      ); // close enough
    let mut packages =
      HashMap::<NpmPackageId, NpmResolutionPackage>::with_capacity(
        snapshot.packages.len(),
      );
    let mut copy_index_resolver =
      SnapshotPackageCopyIndexResolver::with_capacity(snapshot.packages.len());

    // collect the specifiers to version mappings
    for (req, id) in snapshot.root_packages {
      package_reqs.insert(req, id.nv.clone());
      root_packages.insert(id.nv.clone(), id.clone());
    }

    // then the packages
    for package in snapshot.packages {
      packages_by_name
        .entry(package.pkg_id.nv.name.to_string())
        .or_default()
        .push(package.pkg_id.clone());

      let copy_index = copy_index_resolver.resolve(&package.pkg_id);
      packages.insert(
        package.pkg_id.clone(),
        NpmResolutionPackage {
          pkg_id: package.pkg_id,
          copy_index,
          dist: package.dist,
          dependencies: package.dependencies,
        },
      );
    }

    Self {
      api: options.api,
      version_resolver: NpmVersionResolver {
        types_node_version_req: options.types_node_version_req,
      },
      package_reqs,
      root_packages,
      packages_by_name,
      packages,
      pending_unresolved_packages: Default::default(),
    }
  }

  /// Gets the snapshot as a serialized snapshot.
  pub fn as_serialized(&self) -> SerializedNpmResolutionSnapshot {
    SerializedNpmResolutionSnapshot {
      root_packages: self
        .package_reqs
        .iter()
        .map(|(req, nv)| {
          let id = self.root_packages.get(nv).unwrap();
          (req.clone(), id.clone())
        })
        .collect(),
      packages: self
        .packages
        .values()
        .map(|package| SerializedNpmResolutionSnapshotPackage {
          pkg_id: package.pkg_id.clone(),
          dist: package.dist.clone(),
          dependencies: package.dependencies.clone(),
        })
        .collect(),
    }
  }

  /// Gets if this snapshot is empty.
  pub fn is_empty(&self) -> bool {
    self.packages.is_empty() && self.pending_unresolved_packages.is_empty()
  }

  /// Gets if the snapshot has any pending packages whose dependencies
  /// need to be resolved.
  pub fn has_pending(&self) -> bool {
    !self.pending_unresolved_packages.is_empty()
  }

  /// Converts the snapshot into an empty snapshot.
  pub fn into_empty(self) -> Self {
    // this is `into_empty()` instead of something like `clear()` in order
    // to reduce the chance of a mistake forgetting to clear a collection
    Self {
      api: self.api,
      version_resolver: self.version_resolver,
      package_reqs: Default::default(),
      root_packages: Default::default(),
      packages_by_name: Default::default(),
      packages: Default::default(),
      pending_unresolved_packages: Default::default(),
    }
  }

  /// Resolves any pending packages in the snapshot along with the provided
  /// package requirements (in the CLI, these are package requirements from
  /// a package.json while the pending are specifiers found in the graph)
  pub async fn resolve_pending(
    self,
    package_reqs: Vec<NpmPackageReq>,
  ) -> Result<Self, NpmResolutionError> {
    // convert the snapshot to a traversable graph
    let (mut graph, api, version_resolver) = Graph::from_snapshot(self);
    let pending_unresolved = graph.take_pending_unresolved();

    let package_reqs = package_reqs
      .into_iter()
      .filter(|r| !graph.has_package_req(r));
    let pending_unresolved = pending_unresolved
      .into_iter()
      .filter(|p| !graph.has_root_package(p));

    enum ReqOrNv {
      Req(NpmPackageReq),
      Nv(Arc<NpmPackageNv>),
    }

    let mut top_level_packages = futures::stream::FuturesOrdered::from_iter({
      let api = &api;
      package_reqs
        .map(|req| {
          Either::Left(async {
            let info = api.package_info(&req.name).await?;
            Result::<_, NpmRegistryPackageInfoLoadError>::Ok((
              ReqOrNv::Req(req),
              info,
            ))
          })
        })
        .chain(pending_unresolved.map(|nv| {
          Either::Right(async {
            let info = api.package_info(&nv.name).await?;
            Ok((ReqOrNv::Nv(nv), info))
          })
        }))
    });

    // go over the top level package names first (npm package reqs and pending unresolved),
    // then down the tree one level at a time through all the branches
    let mut resolver =
      GraphDependencyResolver::new(&mut graph, &*api, &version_resolver);

    // The package reqs and ids should already be sorted
    // in the order they should be resolved in.
    while let Some(result) = top_level_packages.next().await {
      let (req_or_nv, info) = result?;
      match req_or_nv {
        ReqOrNv::Req(req) => resolver.add_package_req(&req, &info)?,
        ReqOrNv::Nv(nv) => resolver.add_root_package(&nv, &info)?,
      }
    }
    drop(top_level_packages); // stop borrow of api

    resolver.resolve_pending().await?;

    let snapshot = graph.into_snapshot(api, version_resolver).await?;
    debug_assert!(!snapshot.has_pending());
    Ok(snapshot)
  }

  /// Resolve a package from a package requirement.
  pub fn resolve_pkg_from_pkg_req(
    &self,
    req: &NpmPackageReq,
  ) -> Result<&NpmResolutionPackage, PackageReqNotFoundError> {
    match self.package_reqs.get(req) {
      Some(id) => self
        .resolve_package_from_deno_module(id)
        // ignore the nv not found error and return a req not found
        .map_err(|_| PackageReqNotFoundError(req.clone())),
      None => Err(PackageReqNotFoundError(req.clone())),
    }
  }

  /// Resolve a package from a deno module.
  pub fn resolve_package_from_deno_module(
    &self,
    nv: &NpmPackageNv,
  ) -> Result<&NpmResolutionPackage, PackageNvNotFoundError> {
    match self.root_packages.get(nv) {
      Some(id) => Ok(self.packages.get(id).unwrap()),
      None => Err(PackageNvNotFoundError(nv.clone())),
    }
  }

  pub fn top_level_packages(
    &self,
  ) -> hash_map::Values<NpmPackageNv, NpmPackageId> {
    self.root_packages.values()
  }

  pub fn package_reqs(&self) -> &HashMap<NpmPackageReq, NpmPackageNv> {
    &self.package_reqs
  }

  pub fn package_from_id(
    &self,
    id: &NpmPackageId,
  ) -> Option<&NpmResolutionPackage> {
    self.packages.get(id)
  }

  pub fn resolve_package_from_package(
    &self,
    name: &str,
    referrer: &NpmPackageCacheFolderId,
  ) -> Result<&NpmResolutionPackage, Box<PackageNotFoundFromReferrerError>> {
    // todo(dsherret): do we need an additional hashmap to get this quickly?
    let referrer_package = self
      .packages_by_name
      .get(&referrer.nv.name)
      .and_then(|packages| {
        packages
          .iter()
          .filter(|p| p.nv.version == referrer.nv.version)
          .filter_map(|node_id| {
            let package = self.packages.get(node_id)?;
            if package.copy_index == referrer.copy_index {
              Some(package)
            } else {
              None
            }
          })
          .next()
      })
      .ok_or_else(|| {
        Box::new(PackageNotFoundFromReferrerError::Referrer(referrer.clone()))
      })?;

    let name = name_without_path(name);
    if let Some(id) = referrer_package.dependencies.get(name) {
      return Ok(self.packages.get(id).unwrap());
    }

    if referrer_package.pkg_id.nv.name == name {
      return Ok(referrer_package);
    }

    // TODO(bartlomieju): this should use a reverse lookup table in the
    // snapshot instead of resolving best version again.
    let any_version_req = VersionReq::parse_from_npm("*").unwrap();
    if let Some(id) = self.resolve_best_package_id(name, &any_version_req) {
      if let Some(pkg) = self.packages.get(&id) {
        return Ok(pkg);
      }
    }

    Err(Box::new(PackageNotFoundFromReferrerError::Package {
      name: name.to_string(),
      referrer: referrer.clone(),
    }))
  }

  pub fn all_packages(&self) -> Vec<NpmResolutionPackage> {
    self.packages.values().cloned().collect()
  }

  pub fn all_packages_partitioned(&self) -> NpmPackagesPartitioned {
    let mut packages = self.all_packages();
    let mut copy_packages = Vec::with_capacity(packages.len() / 2); // at most 1 copy for every package

    // partition out any packages that are "copy" packages
    for i in (0..packages.len()).rev() {
      if packages[i].copy_index > 0 {
        copy_packages.push(packages.swap_remove(i));
      }
    }

    NpmPackagesPartitioned {
      packages,
      copy_packages,
    }
  }

  pub fn resolve_best_package_id(
    &self,
    name: &str,
    version_req: &VersionReq,
  ) -> Option<NpmPackageId> {
    // todo(dsherret): this is not exactly correct because some ids
    // will be better than others due to peer dependencies
    let mut maybe_best_id: Option<&NpmPackageId> = None;
    if let Some(node_ids) = self.packages_by_name.get(name) {
      for node_id in node_ids.iter() {
        if version_req.matches(&node_id.nv.version) {
          let is_best_version = maybe_best_id
            .as_ref()
            .map(|best_id| best_id.nv.version.cmp(&node_id.nv.version).is_lt())
            .unwrap_or(true);
          if is_best_version {
            maybe_best_id = Some(node_id);
          }
        }
      }
    }
    maybe_best_id.cloned()
  }

  pub fn resolve_package_req_as_pending(
    &mut self,
    pkg_req: &NpmPackageReq,
    package_info: &NpmPackageInfo,
  ) -> Result<NpmPackageNv, NpmPackageVersionResolutionError> {
    let version_req =
      pkg_req.version_req.as_ref().unwrap_or(&*LATEST_VERSION_REQ);
    let version_info = match self.packages_by_name.get(&package_info.name) {
      Some(existing_versions) => {
        self.version_resolver.resolve_best_package_version_info(
          version_req,
          package_info,
          existing_versions.iter().map(|p| &p.nv.version),
        )?
      }
      None => self.version_resolver.resolve_best_package_version_info(
        version_req,
        package_info,
        Vec::new().iter(),
      )?,
    };
    let nv = NpmPackageNv {
      name: package_info.name.to_string(),
      version: version_info.version.clone(),
    };
    debug!(
      "Resolved {}@{} to {}",
      pkg_req.name,
      version_req.version_text(),
      nv,
    );
    self.add_pending_pkg(pkg_req.clone(), nv.clone());
    Ok(nv)
  }

  fn add_pending_pkg(&mut self, pkg_req: NpmPackageReq, nv: NpmPackageNv) {
    self.package_reqs.insert(pkg_req, nv.clone());
    let packages_with_name =
      self.packages_by_name.entry(nv.name.clone()).or_default();
    if !packages_with_name.iter().any(|p| p.nv == nv) {
      packages_with_name.push(NpmPackageId {
        nv: nv.clone(),
        peer_dependencies: Vec::new(),
      });
    }
    self.pending_unresolved_packages.push(nv);
  }
}

pub struct SnapshotPackageCopyIndexResolver {
  packages_to_copy_index: HashMap<NpmPackageId, u8>,
  package_name_version_to_copy_count: HashMap<NpmPackageNv, u8>,
}

impl SnapshotPackageCopyIndexResolver {
  pub fn with_capacity(capacity: usize) -> Self {
    Self {
      packages_to_copy_index: HashMap::with_capacity(capacity),
      package_name_version_to_copy_count: HashMap::with_capacity(capacity), // close enough
    }
  }

  pub fn from_map_with_capacity(
    mut packages_to_copy_index: HashMap<NpmPackageId, u8>,
    capacity: usize,
  ) -> Self {
    let mut package_name_version_to_copy_count =
      HashMap::with_capacity(capacity); // close enough
    if capacity > packages_to_copy_index.len() {
      packages_to_copy_index.reserve(capacity - packages_to_copy_index.len());
    }

    for (node_id, index) in &packages_to_copy_index {
      let entry = package_name_version_to_copy_count
        .entry(node_id.nv.clone())
        .or_insert(0);
      if *entry < *index {
        *entry = *index;
      }
    }
    Self {
      packages_to_copy_index,
      package_name_version_to_copy_count,
    }
  }

  pub fn resolve(&mut self, node_id: &NpmPackageId) -> u8 {
    if let Some(index) = self.packages_to_copy_index.get(node_id) {
      *index
    } else {
      let index = *self
        .package_name_version_to_copy_count
        .entry(node_id.nv.clone())
        .and_modify(|count| {
          *count += 1;
        })
        .or_insert(0);
      self.packages_to_copy_index.insert(node_id.clone(), index);
      index
    }
  }
}

fn name_without_path(name: &str) -> &str {
  let mut search_start_index = 0;
  if name.starts_with('@') {
    if let Some(slash_index) = name.find('/') {
      search_start_index = slash_index + 1;
    }
  }
  if let Some(slash_index) = &name[search_start_index..].find('/') {
    // get the name up until the path slash
    &name[0..search_start_index + slash_index]
  } else {
    name
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_name_without_path() {
    assert_eq!(name_without_path("foo"), "foo");
    assert_eq!(name_without_path("@foo/bar"), "@foo/bar");
    assert_eq!(name_without_path("@foo/bar/baz"), "@foo/bar");
    assert_eq!(name_without_path("@hello"), "@hello");
  }

  #[test]
  fn test_copy_index_resolver() {
    let mut copy_index_resolver =
      SnapshotPackageCopyIndexResolver::with_capacity(10);
    assert_eq!(
      copy_index_resolver
        .resolve(&NpmPackageId::from_serialized("package@1.0.0").unwrap()),
      0
    );
    assert_eq!(
      copy_index_resolver
        .resolve(&NpmPackageId::from_serialized("package@1.0.0").unwrap()),
      0
    );
    assert_eq!(
      copy_index_resolver.resolve(
        &NpmPackageId::from_serialized("package@1.0.0_package-b@1.0.0")
          .unwrap()
      ),
      1
    );
    assert_eq!(
      copy_index_resolver.resolve(
        &NpmPackageId::from_serialized(
          "package@1.0.0_package-b@1.0.0__package-c@2.0.0"
        )
        .unwrap()
      ),
      2
    );
    assert_eq!(
      copy_index_resolver.resolve(
        &NpmPackageId::from_serialized("package@1.0.0_package-b@1.0.0")
          .unwrap()
      ),
      1
    );
    assert_eq!(
      copy_index_resolver
        .resolve(&NpmPackageId::from_serialized("package-b@1.0.0").unwrap()),
      0
    );
  }
}
