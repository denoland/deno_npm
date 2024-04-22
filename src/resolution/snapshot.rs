// Copyright 2018-2024 the Deno authors. MIT license.

use std::collections::hash_map;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::path::PathBuf;
use std::rc::Rc;

use deno_lockfile::IntegrityCheckFailedError;
use deno_lockfile::Lockfile;
use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use deno_semver::package::PackageReqParseError;
use deno_semver::VersionReq;
use futures::future::Either;
use futures::stream::FuturesOrdered;
use futures::StreamExt;
use log::debug;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

use super::common::NpmVersionResolver;
use super::graph::Graph;
use super::graph::GraphDependencyResolver;
use super::graph::NpmResolutionError;
use super::NpmPackageVersionNotFound;
use super::NpmPackageVersionResolutionError;

use crate::registry::NpmPackageInfo;
use crate::registry::NpmPackageVersionDistInfo;
use crate::registry::NpmRegistryApi;
use crate::registry::NpmRegistryPackageInfoLoadError;
use crate::NpmPackageCacheFolderId;
use crate::NpmPackageId;
use crate::NpmPackageIdDeserializationError;
use crate::NpmResolutionPackage;
use crate::NpmResolutionPackageSystemInfo;
use crate::NpmSystemInfo;

#[derive(Debug, Error, Clone)]
#[error("Could not find '{}' in the list of packages.", self.0.as_serialized())]
pub struct PackageIdNotFoundError(pub NpmPackageId);

#[derive(Debug, Error, Clone)]
#[error("Could not find constraint '{0}' in the list of packages.")]
pub struct PackageReqNotFoundError(pub PackageReq);

#[derive(Debug, Error, Clone)]
#[error("Could not find '{0}' in the list of packages.")]
pub struct PackageNvNotFoundError(pub PackageNv);

#[derive(Debug, Error, Clone)]
#[error("Could not find package folder id '{0}' in the list of packages.")]
pub struct PackageCacheFolderIdNotFoundError(pub NpmPackageCacheFolderId);

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
  pub fn iter_all(&self) -> impl Iterator<Item = &NpmResolutionPackage> {
    self.packages.iter().chain(self.copy_packages.iter())
  }
}

/// A serialized snapshot that has been verified to be non-corrupt
/// and valid.
#[derive(Debug, Default, Clone)]
pub struct ValidSerializedNpmResolutionSnapshot(
  // keep private -- once verified the caller
  // shouldn't be able to modify it
  SerializedNpmResolutionSnapshot,
);

impl ValidSerializedNpmResolutionSnapshot {
  pub fn as_serialized(&self) -> &SerializedNpmResolutionSnapshot {
    &self.0
  }

  pub fn into_serialized(self) -> SerializedNpmResolutionSnapshot {
    self.0
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SerializedNpmResolutionSnapshotPackage {
  pub id: NpmPackageId,
  #[serde(flatten)]
  pub system: NpmResolutionPackageSystemInfo,
  pub dist: NpmPackageVersionDistInfo,
  /// Key is what the package refers to the other package as,
  /// which could be different from the package name.
  pub dependencies: HashMap<String, NpmPackageId>,
  pub optional_dependencies: HashSet<String>,
}

#[derive(Default, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SerializedNpmResolutionSnapshot {
  /// Resolved npm specifiers to package id mappings.
  pub root_packages: HashMap<PackageReq, NpmPackageId>,
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
      package_ids.insert(&package.id);
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

  /// Trusts that the serialized snapshot is valid and skips runtime verification
  /// that is done in `into_valid`.
  ///
  /// Note: It will still do the verification in debug.
  pub fn into_valid_unsafe(self) -> ValidSerializedNpmResolutionSnapshot {
    if cfg!(debug) {
      self.into_valid().unwrap()
    } else {
      ValidSerializedNpmResolutionSnapshot(self)
    }
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

#[derive(Debug, Clone)]
pub struct NpmResolutionSnapshot {
  /// The unique package requirements map to a single npm package name and version.
  pub(super) package_reqs: HashMap<PackageReq, PackageNv>,
  // Each root level npm package name and version maps to an exact npm package node id.
  pub(super) root_packages: HashMap<PackageNv, NpmPackageId>,
  pub(super) packages_by_name: HashMap<String, Vec<NpmPackageId>>,
  pub(super) packages: HashMap<NpmPackageId, NpmResolutionPackage>,
  /// Ordered list based on resolution of packages whose dependencies
  /// have not yet been resolved
  pub(super) pending_unresolved_packages: Vec<PackageNv>,
}

impl NpmResolutionSnapshot {
  pub fn new(snapshot: ValidSerializedNpmResolutionSnapshot) -> Self {
    let snapshot = snapshot.0;
    let mut package_reqs = HashMap::<PackageReq, PackageNv>::with_capacity(
      snapshot.root_packages.len(),
    );
    let mut root_packages = HashMap::<PackageNv, NpmPackageId>::with_capacity(
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
        .entry(package.id.nv.name.to_string())
        .or_default()
        .push(package.id.clone());

      let copy_index = copy_index_resolver.resolve(&package.id);
      packages.insert(
        package.id.clone(),
        NpmResolutionPackage {
          id: package.id,
          copy_index,
          system: package.system,
          dist: package.dist,
          dependencies: package.dependencies,
          optional_dependencies: package.optional_dependencies,
        },
      );
    }

    Self {
      package_reqs,
      root_packages,
      packages_by_name,
      packages,
      pending_unresolved_packages: Default::default(),
    }
  }

  /// Gets the snapshot as a valid serialized snapshot.
  pub fn as_valid_serialized(&self) -> ValidSerializedNpmResolutionSnapshot {
    ValidSerializedNpmResolutionSnapshot(SerializedNpmResolutionSnapshot {
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
        .map(|package| package.as_serialized())
        .collect(),
    })
  }

  /// Filters out any optional dependencies that don't match for the
  /// given system. The resulting valid serialized snapshot will then not
  /// have any optional dependencies that don't match the given system.
  pub fn as_valid_serialized_for_system(
    &self,
    system_info: &NpmSystemInfo,
  ) -> ValidSerializedNpmResolutionSnapshot {
    let mut final_packages = Vec::with_capacity(self.packages.len());
    let mut pending = VecDeque::with_capacity(self.packages.len());
    let mut visited_ids = HashSet::with_capacity(self.packages.len());

    // add the root packages
    for pkg_id in self.root_packages.values() {
      if visited_ids.insert(pkg_id) {
        pending.push_back(self.packages.get(pkg_id).unwrap());
      }
    }

    while let Some(pkg) = pending.pop_front() {
      let mut new_pkg = SerializedNpmResolutionSnapshotPackage {
        id: pkg.id.clone(),
        dist: pkg.dist.clone(),
        dependencies: HashMap::with_capacity(pkg.dependencies.len()),
        // the fields below are stripped from the output
        system: Default::default(),
        optional_dependencies: Default::default(),
      };
      for (key, dep_id) in &pkg.dependencies {
        let dep = self.packages.get(dep_id).unwrap();

        let matches_system = !pkg.optional_dependencies.contains(key)
          || dep.system.matches_system(system_info);
        if matches_system {
          new_pkg.dependencies.insert(key.clone(), dep_id.clone());
          if visited_ids.insert(dep_id) {
            pending.push_back(dep);
          }
        }
      }
      final_packages.push(new_pkg);
    }

    ValidSerializedNpmResolutionSnapshot(SerializedNpmResolutionSnapshot {
      packages: final_packages,
      // the root packages are always included since they're
      // what the user imports
      root_packages: self
        .package_reqs
        .iter()
        .map(|(req, nv)| {
          let id = self.root_packages.get(nv).unwrap();
          (req.clone(), id.clone())
        })
        .collect(),
    })
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
      package_reqs: Default::default(),
      root_packages: Default::default(),
      packages_by_name: Default::default(),
      packages: Default::default(),
      pending_unresolved_packages: Default::default(),
    }
  }

  /// Resolve a package from a package requirement.
  pub fn resolve_pkg_from_pkg_req(
    &self,
    req: &PackageReq,
  ) -> Result<&NpmResolutionPackage, PackageReqNotFoundError> {
    match self.package_reqs.get(req) {
      Some(id) => self
        .resolve_package_from_deno_module(id)
        // ignore the nv not found error and return a req not found
        .map_err(|_| PackageReqNotFoundError(req.clone())),
      None => Err(PackageReqNotFoundError(req.clone())),
    }
  }

  /// Resolve a package from a package cache folder id.
  pub fn resolve_pkg_from_pkg_cache_folder_id(
    &self,
    pkg_cache_folder_id: &NpmPackageCacheFolderId,
  ) -> Result<&NpmResolutionPackage, PackageCacheFolderIdNotFoundError> {
    self
      .packages_by_name
      .get(&pkg_cache_folder_id.nv.name)
      .and_then(|ids| {
        for id in ids {
          if id.nv == pkg_cache_folder_id.nv {
            if let Some(pkg) = self.packages.get(id) {
              if pkg.copy_index == pkg_cache_folder_id.copy_index {
                return Some(pkg);
              }
            }
          }
        }
        None
      })
      .map(Ok)
      .unwrap_or_else(|| {
        Err(PackageCacheFolderIdNotFoundError(
          pkg_cache_folder_id.clone(),
        ))
      })
  }

  /// Resolve a package from a deno module.
  pub fn resolve_package_from_deno_module(
    &self,
    nv: &PackageNv,
  ) -> Result<&NpmResolutionPackage, PackageNvNotFoundError> {
    match self.root_packages.get(nv) {
      Some(id) => Ok(self.packages.get(id).unwrap()),
      None => Err(PackageNvNotFoundError(nv.clone())),
    }
  }

  pub fn top_level_packages(
    &self,
  ) -> hash_map::Values<PackageNv, NpmPackageId> {
    self.root_packages.values()
  }

  pub fn package_reqs(&self) -> &HashMap<PackageReq, PackageNv> {
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

    if referrer_package.id.nv.name == name {
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

  /// Gets all the packages found in the snapshot regardless of
  /// whether they are supported on the current system.
  pub fn all_packages_for_every_system(
    &self,
  ) -> impl Iterator<Item = &NpmResolutionPackage> {
    // NOTE: This method intentionally has a verbose name
    // to discourage its use.
    self.packages.values()
  }

  pub fn all_system_packages(
    &self,
    system_info: &NpmSystemInfo,
  ) -> Vec<NpmResolutionPackage> {
    let mut packages = Vec::with_capacity(self.packages.len());
    let mut pending = VecDeque::with_capacity(self.packages.len());
    let mut visited_ids = HashSet::with_capacity(self.packages.len());

    for pkg_id in self.root_packages.values() {
      if visited_ids.insert(pkg_id) {
        pending.push_back(self.packages.get(pkg_id).unwrap());
      }
    }

    while let Some(pkg) = pending.pop_front() {
      packages.push(pkg.clone());

      for (key, dep_id) in &pkg.dependencies {
        if visited_ids.contains(&dep_id) {
          continue;
        }
        let dep = self.packages.get(dep_id).unwrap();

        let matches_system = !pkg.optional_dependencies.contains(key)
          || dep.system.matches_system(system_info);
        if matches_system {
          pending.push_back(dep);
          visited_ids.insert(dep_id);
        }
      }
    }

    packages
  }

  pub fn all_system_packages_partitioned(
    &self,
    system_info: &NpmSystemInfo,
  ) -> NpmPackagesPartitioned {
    let mut packages = self.all_system_packages(system_info);

    // in most scenarios, there won't ever be any copy packages so skip
    // the extra allocations if so
    let copy_packages = if packages.iter().any(|p| p.copy_index > 0) {
      let mut copy_packages = Vec::with_capacity(packages.len() / 2); // at most 1 copy for every package
      let copy_index_zero_nvs = packages
        .iter()
        .filter(|p| p.copy_index == 0)
        .map(|p| p.id.nv.clone())
        .collect::<HashSet<_>>();

      // partition out any packages that are "copy" packages
      for i in (0..packages.len()).rev() {
        if packages[i].copy_index > 0
          // the system might not have resolved the package with a
          // copy_index of 0, so we also need to check that
          && copy_index_zero_nvs.contains(&packages[i].id.nv)
        {
          copy_packages.push(packages.swap_remove(i));
        }
      }
      copy_packages
    } else {
      Vec::new()
    };

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

  fn add_pending_pkg(&mut self, pkg_req: PackageReq, nv: PackageNv) {
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
  package_name_version_to_copy_count: HashMap<PackageNv, u8>,
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

pub struct NpmResolutionSnapshotPendingResolverOptions<
  'a,
  TNpmRegistryApi: NpmRegistryApi,
> {
  pub api: &'a TNpmRegistryApi,
  /// Known good version requirement to use for the `@types/node` package
  /// when the version is unspecified or "latest".
  pub types_node_version_req: Option<VersionReq>,
}

/// Resolves pending packages in the npm snapshot.
pub struct NpmResolutionSnapshotPendingResolver<
  'a,
  TNpmRegistryApi: NpmRegistryApi,
> {
  version_resolver: NpmVersionResolver,
  api: &'a TNpmRegistryApi,
}

impl<'a, TNpmRegistryApi: NpmRegistryApi>
  NpmResolutionSnapshotPendingResolver<'a, TNpmRegistryApi>
{
  pub fn new(
    options: NpmResolutionSnapshotPendingResolverOptions<'a, TNpmRegistryApi>,
  ) -> Self {
    Self {
      api: options.api,
      version_resolver: NpmVersionResolver {
        types_node_version_req: options.types_node_version_req,
      },
    }
  }

  pub fn resolve_package_req_as_pending(
    &self,
    snapshot: &mut NpmResolutionSnapshot,
    pkg_req: &PackageReq,
    package_info: &NpmPackageInfo,
  ) -> Result<PackageNv, NpmPackageVersionResolutionError> {
    let version_req = &pkg_req.version_req;
    let nv = if let Some(nv) = snapshot.package_reqs.get(pkg_req) {
      // if a version requirement was previously resolved, don't resolve it again
      nv.clone()
    } else {
      let version_info = match snapshot.packages_by_name.get(&package_info.name)
      {
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
      let nv = PackageNv {
        name: package_info.name.to_string(),
        version: version_info.version.clone(),
      };
      snapshot.add_pending_pkg(pkg_req.clone(), nv.clone());
      nv
    };
    debug!(
      "Resolved {}@{} to {}",
      pkg_req.name,
      version_req.version_text(),
      nv,
    );
    Ok(nv)
  }

  /// Resolves any pending packages in the snapshot along with the provided
  /// package requirements (in the CLI, these are package requirements from
  /// a package.json while the pending are specifiers found in the graph)
  pub async fn resolve_pending(
    &self,
    snapshot: NpmResolutionSnapshot,
    package_reqs: &[PackageReq],
  ) -> Result<NpmResolutionSnapshot, NpmResolutionError> {
    // convert the snapshot to a traversable graph
    let mut graph = Graph::from_snapshot(snapshot);
    let pending_unresolved = graph.take_pending_unresolved();

    let package_reqs =
      package_reqs.iter().filter(|r| !graph.has_package_req(r));
    let pending_unresolved = pending_unresolved
      .into_iter()
      .filter(|p| !graph.has_root_package(p));

    enum ReqOrNv<'a> {
      Req(&'a PackageReq),
      Nv(Rc<PackageNv>),
    }

    let mut top_level_packages = futures::stream::FuturesOrdered::from_iter({
      let api = &self.api;
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
    let mut resolver = GraphDependencyResolver::new(
      &mut graph,
      self.api,
      &self.version_resolver,
    );

    // The package reqs and ids should already be sorted
    // in the order they should be resolved in.
    while let Some(result) = top_level_packages.next().await {
      let (req_or_nv, info) = result?;
      match req_or_nv {
        ReqOrNv::Req(req) => resolver.add_package_req(req, &info)?,
        ReqOrNv::Nv(nv) => resolver.add_root_package(&nv, &info)?,
      }
    }
    drop(top_level_packages); // stop borrow of api

    resolver.resolve_pending().await?;

    let snapshot = graph.into_snapshot(self.api).await?;
    debug_assert!(!snapshot.has_pending());
    Ok(snapshot)
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

#[derive(Debug, Error)]
pub enum IncompleteSnapshotFromLockfileError {
  #[error("Unable to parse npm specifier: {key}")]
  ReqParse {
    key: String,
    #[source]
    source: PackageReqParseError,
  },
  #[error(transparent)]
  PackageIdDeserialization(#[from] NpmPackageIdDeserializationError),
}

struct IncompletePackageInfo {
  id: NpmPackageId,
  integrity: String,
  dependencies: HashMap<String, NpmPackageId>,
}

pub struct IncompleteSnapshot {
  lockfile_file_name: PathBuf,
  root_packages: HashMap<PackageReq, NpmPackageId>,
  packages: Vec<IncompletePackageInfo>,
}

/// Constructs [`IncompleteSnapshot`] from the given [`Lockfile`]. The returned
/// snapshot will then be passed to [`snapshot_from_lockfile`] to get a completely
/// resolved snapshot.
///
/// The reason why this function is not combined with [`snapshot_from_lockfile`]
/// is because we want `lockfile` to not live across the `.await` points. This
/// becomes problematic if `lockfile` is wrapped in [`std::sync::Mutex`] or
/// similar, which doesn't implement [`Send`].
pub fn incomplete_snapshot_from_lockfile(
  lockfile: &Lockfile,
) -> Result<IncompleteSnapshot, IncompleteSnapshotFromLockfileError> {
  let mut root_packages = HashMap::<PackageReq, NpmPackageId>::with_capacity(
    lockfile.content.packages.specifiers.len(),
  );
  // collect the specifiers to version mappings
  for (key, value) in &lockfile.content.packages.specifiers {
    if let Some(key) = key.strip_prefix("npm:") {
      if let Some(value) = value.strip_prefix("npm:") {
        let package_req = PackageReq::from_str(key).map_err(|e| {
          IncompleteSnapshotFromLockfileError::ReqParse {
            key: key.to_string(),
            source: e,
          }
        })?;
        let package_id = NpmPackageId::from_serialized(value)?;
        root_packages.insert(package_req, package_id.clone());
      }
    }
  }

  // now fill the packages except for the dist information
  let mut packages = Vec::with_capacity(lockfile.content.packages.npm.len());
  for (key, package) in &lockfile.content.packages.npm {
    let id = NpmPackageId::from_serialized(key)?;

    // collect the dependencies
    let mut dependencies = HashMap::with_capacity(package.dependencies.len());
    for (name, specifier) in &package.dependencies {
      let dep_id = NpmPackageId::from_serialized(specifier)?;
      dependencies.insert(name.clone(), dep_id);
    }

    packages.push(IncompletePackageInfo {
      id,
      integrity: package.integrity.clone(),
      dependencies,
    });
  }

  Ok(IncompleteSnapshot {
    lockfile_file_name: lockfile.filename.clone(),
    root_packages,
    packages,
  })
}

#[derive(Debug, Error)]
pub enum SnapshotFromLockfileError {
  #[error(transparent)]
  PackageInfoLoad(#[from] NpmRegistryPackageInfoLoadError),
  #[error("Could not find '{}' specified in the lockfile.", .source.0)]
  VersionNotFound {
    #[from]
    source: NpmPackageVersionNotFound,
  },
  #[error("The lockfile is corrupt. You can recreate it with --lock-write")]
  PackageIdNotFound(#[from] PackageIdNotFoundError),
  #[error(transparent)]
  IntegrityCheckFailed(#[from] IntegrityCheckFailedError),
}

pub struct SnapshotFromLockfileParams<'a> {
  pub api: &'a dyn NpmRegistryApi,
  pub incomplete_snapshot: IncompleteSnapshot,
  pub skip_integrity_check: bool,
}

/// Constructs [`ValidSerializedNpmResolutionSnapshot`] from the given [`Lockfile`].
///
/// You should call [`incomplete_snapshot_from_lockfile`] first to get an
/// [`IncompleteSnapshot`] instance that's passed as the first argument for this
/// function.
#[allow(clippy::needless_lifetimes)] // clippy bug
pub async fn snapshot_from_lockfile<'a>(
  params: SnapshotFromLockfileParams<'a>,
) -> Result<ValidSerializedNpmResolutionSnapshot, SnapshotFromLockfileError> {
  let api = params.api;
  let incomplete_snapshot = params.incomplete_snapshot;

  // fetch the package version information
  let pkg_nvs = incomplete_snapshot
    .packages
    .iter()
    .map(|p| p.id.nv.clone())
    .collect::<Vec<_>>();
  let get_version_infos = || {
    FuturesOrdered::from_iter(pkg_nvs.iter().map(|nv| async move {
      let package_info = api
        .package_info(&nv.name)
        .await
        .map_err(SnapshotFromLockfileError::PackageInfoLoad)?;
      package_info
        .version_info(nv)
        .map_err(|e| SnapshotFromLockfileError::VersionNotFound { source: e })
    }))
  };
  let mut version_infos = get_version_infos();
  let mut i = 0;
  let mut packages = Vec::with_capacity(incomplete_snapshot.packages.len());
  while let Some(result) = version_infos.next().await {
    match result {
      Ok(version_info) => {
        let snapshot_package = &incomplete_snapshot.packages[i];
        if !params.skip_integrity_check {
          let registry_integrity = version_info.dist.integrity().for_lockfile();
          if registry_integrity != snapshot_package.integrity {
            return Err(
              IntegrityCheckFailedError {
                package_display_id: format!(
                  "npm:{}",
                  snapshot_package.id.as_serialized()
                ),
                expected: snapshot_package.integrity.clone(),
                actual: registry_integrity,
                filename: incomplete_snapshot
                  .lockfile_file_name
                  .display()
                  .to_string(),
              }
              .into(),
            );
          }
        }
        packages.push(SerializedNpmResolutionSnapshotPackage {
          id: snapshot_package.id.clone(),
          dependencies: snapshot_package.dependencies.clone(),
          dist: version_info.dist,
          system: NpmResolutionPackageSystemInfo {
            cpu: version_info.cpu,
            os: version_info.os,
          },
          optional_dependencies: version_info
            .optional_dependencies
            .into_keys()
            .collect(),
        });
      }
      Err(err) => {
        if api.mark_force_reload() {
          // reset and try again
          version_infos = get_version_infos();
          i = 0;
          packages.clear();
          continue;
        } else {
          return Err(err);
        }
      }
    }

    i += 1;
  }

  let snapshot = SerializedNpmResolutionSnapshot {
    packages,
    root_packages: incomplete_snapshot.root_packages,
  }
  .into_valid()?;
  Ok(snapshot)
}

#[cfg(test)]
mod tests {
  use std::path::PathBuf;

  use deno_semver::Version;
  use pretty_assertions::assert_eq;

  use crate::registry::TestNpmRegistryApi;

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

  #[test]
  fn test_as_valid_serialized_for_system() {
    let original_serialized = SerializedNpmResolutionSnapshot {
      root_packages: root_pkgs(&[("a@1", "a@1.0.0")]),
      packages: vec![
        SerializedNpmResolutionSnapshotPackage {
          id: NpmPackageId::from_serialized("a@1.0.0").unwrap(),
          dependencies: deps(&[("b", "b@1.0.0"), ("c", "c@1.0.0")]),
          system: Default::default(),
          dist: Default::default(),
          optional_dependencies: HashSet::from(["c".to_string()]),
        },
        SerializedNpmResolutionSnapshotPackage {
          id: NpmPackageId::from_serialized("b@1.0.0").unwrap(),
          dependencies: Default::default(),
          system: Default::default(),
          dist: Default::default(),
          optional_dependencies: Default::default(),
        },
        SerializedNpmResolutionSnapshotPackage {
          id: NpmPackageId::from_serialized("c@1.0.0").unwrap(),
          dependencies: deps(&[("b", "b@1.0.0"), ("d", "d@1.0.0")]),
          system: NpmResolutionPackageSystemInfo {
            os: vec!["win32".to_string()],
            cpu: vec!["x64".to_string()],
          },
          dist: Default::default(),
          optional_dependencies: Default::default(),
        },
        SerializedNpmResolutionSnapshotPackage {
          id: NpmPackageId::from_serialized("d@1.0.0").unwrap(),
          dependencies: Default::default(),
          system: Default::default(),
          dist: Default::default(),
          optional_dependencies: Default::default(),
        },
      ],
    }
    .into_valid()
    .unwrap();
    let snapshot = NpmResolutionSnapshot::new(original_serialized.clone());
    // test providing a matching system
    {
      let mut actual = snapshot
        .as_valid_serialized_for_system(&NpmSystemInfo {
          os: "win32".to_string(),
          cpu: "x64".to_string(),
        })
        .into_serialized();
      actual.packages.sort_by(|a, b| a.id.cmp(&b.id));
      let mut expected = original_serialized.clone().into_serialized();
      for pkg in expected.packages.iter_mut() {
        pkg.system = Default::default();
        pkg.optional_dependencies.clear();
      }
      expected.packages.sort_by(|a, b| a.id.cmp(&b.id));
      assert_eq!(actual, expected);
    }
    // test providing a non-matching system
    {
      let mut actual = snapshot
        .as_valid_serialized_for_system(&NpmSystemInfo {
          os: "darwin".to_string(),
          cpu: "x64".to_string(),
        })
        .into_serialized();
      actual.packages.sort_by(|a, b| a.id.cmp(&b.id));
      let mut expected = original_serialized.into_serialized();
      for pkg in expected.packages.iter_mut() {
        pkg.system = Default::default();
        pkg.optional_dependencies.clear();
      }
      expected.packages.sort_by(|a, b| a.id.cmp(&b.id));
      // these are sorted, so remove the c and d packages
      expected.packages.remove(3);
      expected.packages.remove(2);
      // remove c as a dependency from a
      assert!(expected.packages[0].dependencies.remove("c").is_some());
      assert_eq!(actual, expected);
    }
  }

  #[test]
  fn resolve_pkg_from_pkg_cache_folder_id() {
    let original_serialized = SerializedNpmResolutionSnapshot {
      root_packages: root_pkgs(&[("a@1", "a@1.0.0")]),
      packages: vec![
        pkg_with_id("a@1.0.0"),
        pkg_with_id("a@1.0.0_b@1.0.0"),
        pkg_with_id("a@1.1.0"),
        pkg_with_id("b@1.0.0"),
      ],
    }
    .into_valid()
    .unwrap();
    let snapshot = NpmResolutionSnapshot::new(original_serialized);

    let pkg = snapshot
      .resolve_pkg_from_pkg_cache_folder_id(&npm_cache_folder_id(
        "a", "1.0.0", 0,
      ))
      .unwrap();
    assert_eq!(pkg.id.as_serialized(), "a@1.0.0");
    assert_eq!(pkg.copy_index, 0);

    let pkg = snapshot
      .resolve_pkg_from_pkg_cache_folder_id(&npm_cache_folder_id(
        "a", "1.0.0", 1,
      ))
      .unwrap();
    assert_eq!(pkg.id.as_serialized(), "a@1.0.0_b@1.0.0");
    assert_eq!(pkg.copy_index, 1);
    assert!(snapshot
      .resolve_pkg_from_pkg_cache_folder_id(&npm_cache_folder_id(
        "a", "1.0.0", 2,
      ))
      .is_err());
    assert!(snapshot
      .resolve_pkg_from_pkg_cache_folder_id(&npm_cache_folder_id(
        "b", "1.0.0", 2,
      ))
      .is_err());
  }

  fn npm_cache_folder_id(
    name: &str,
    version: &str,
    copy_index: u8,
  ) -> NpmPackageCacheFolderId {
    NpmPackageCacheFolderId {
      nv: PackageNv {
        name: name.to_string(),
        version: Version::parse_standard(version).unwrap(),
      },
      copy_index,
    }
  }

  fn pkg_with_id(id: &str) -> SerializedNpmResolutionSnapshotPackage {
    SerializedNpmResolutionSnapshotPackage {
      id: NpmPackageId::from_serialized(id).unwrap(),
      dependencies: Default::default(),
      system: Default::default(),
      dist: Default::default(),
      optional_dependencies: Default::default(),
    }
  }

  fn deps(deps: &[(&str, &str)]) -> HashMap<String, NpmPackageId> {
    deps
      .iter()
      .map(|(key, value)| {
        (
          key.to_string(),
          NpmPackageId::from_serialized(value).unwrap(),
        )
      })
      .collect()
  }

  fn root_pkgs(pkgs: &[(&str, &str)]) -> HashMap<PackageReq, NpmPackageId> {
    pkgs
      .iter()
      .map(|(key, value)| {
        (
          PackageReq::from_str(key).unwrap(),
          NpmPackageId::from_serialized(value).unwrap(),
        )
      })
      .collect()
  }

  #[tokio::test]
  async fn test_snapshot_from_lockfile_v2() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version_with_integrity(
      "chalk",
      "5.3.0",
      Some("sha512-integrity1"),
    );
    api.ensure_package_version_with_integrity(
      "emoji-regex",
      "10.2.1",
      Some("sha512-integrity2"),
    );

    let lockfile = Lockfile::with_lockfile_content(
      PathBuf::from("/deno.lock"),
      r#"{
        "version": "2",
        "remote": {},
        "npm": {
          "specifiers": {
            "chalk@5": "chalk@5.3.0",
            "emoji-regex": "emoji-regex@10.2.1"
          },
          "packages": {
            "chalk@5.3.0": {
              "integrity": "sha512-integrity1",
              "dependencies": {}
            },
            "emoji-regex@10.2.1": {
              "integrity": "sha512-integrity2",
              "dependencies": {}
            }
          }
        }
      }"#,
      false,
    )
    .unwrap();

    let incomplete_snapshot =
      incomplete_snapshot_from_lockfile(&lockfile).unwrap();
    assert!(snapshot_from_lockfile(SnapshotFromLockfileParams {
      incomplete_snapshot,
      api: &api,
      skip_integrity_check: false
    })
    .await
    .is_ok());
  }

  #[tokio::test]
  async fn test_snapshot_from_lockfile_bad_integrity() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version_with_integrity(
      "chalk",
      "5.3.0",
      Some("sha512-integrity1-bad"),
    );
    api.ensure_package_version_with_integrity(
      "emoji-regex",
      "10.2.1",
      Some("sha512-integrity2"),
    );

    let lockfile = Lockfile::with_lockfile_content(
      PathBuf::from("/deno.lock"),
      r#"{
        "version": "2",
        "remote": {},
        "npm": {
          "specifiers": {
            "chalk@5": "chalk@5.3.0",
            "emoji-regex": "emoji-regex@10.2.1"
          },
          "packages": {
            "chalk@5.3.0": {
              "integrity": "sha512-integrity1",
              "dependencies": {}
            },
            "emoji-regex@10.2.1": {
              "integrity": "sha512-integrity2",
              "dependencies": {}
            }
          }
        }
      }"#,
      false,
    )
    .unwrap();

    let incomplete_snapshot =
      incomplete_snapshot_from_lockfile(&lockfile).unwrap();
    let err = snapshot_from_lockfile(SnapshotFromLockfileParams {
      incomplete_snapshot,
      api: &api,
      skip_integrity_check: false,
    })
    .await
    .unwrap_err();
    match err {
      SnapshotFromLockfileError::IntegrityCheckFailed(err) => {
        assert_eq!(err.actual, "sha512-integrity1-bad");
        assert_eq!(err.expected, "sha512-integrity1");
        assert_eq!(err.filename, "/deno.lock");
        assert_eq!(err.package_display_id, "npm:chalk@5.3.0");
      }
      _ => unreachable!(),
    }

    // now try with skipping the integrity check
    let incomplete_snapshot =
      incomplete_snapshot_from_lockfile(&lockfile).unwrap();
    assert!(snapshot_from_lockfile(SnapshotFromLockfileParams {
      incomplete_snapshot,
      api: &api,
      skip_integrity_check: true, // will pass because ignored
    })
    .await
    .is_ok());
  }

  #[tokio::test]
  async fn test_snapshot_from_lockfile_v3() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version_with_integrity(
      "chalk",
      "5.3.0",
      Some("sha512-integrity1"),
    );
    api.ensure_package_version_with_integrity(
      "emoji-regex",
      "10.2.1",
      Some("sha512-integrity2"),
    );

    let lockfile = Lockfile::with_lockfile_content(
      PathBuf::from("/deno.lock"),
      r#"{
        "version": "3",
        "remote": {},
        "packages": {
          "specifiers": {
            "npm:chalk@5": "npm:chalk@5.3.0",
            "npm:emoji-regex": "npm:emoji-regex@10.2.1",
            "deno:path": "deno:@std/path@1.0.0"
          },
          "npm": {
            "chalk@5.3.0": {
              "integrity": "sha512-integrity1",
              "dependencies": {}
            },
            "emoji-regex@10.2.1": {
              "integrity": "sha512-integrity2",
              "dependencies": {}
            }
          }
        }
      }"#,
      false,
    )
    .unwrap();

    let incomplete_snapshot =
      incomplete_snapshot_from_lockfile(&lockfile).unwrap();
    let snapshot = snapshot_from_lockfile(SnapshotFromLockfileParams {
      incomplete_snapshot,
      api: &api,
      skip_integrity_check: false,
    })
    .await
    .unwrap();
    assert_eq!(
      snapshot.as_serialized().root_packages,
      HashMap::from([
        (
          PackageReq::from_str("chalk@5").unwrap(),
          NpmPackageId::from_serialized("chalk@5.3.0").unwrap()
        ),
        (
          PackageReq::from_str("emoji-regex").unwrap(),
          NpmPackageId::from_serialized("emoji-regex@10.2.1").unwrap()
        )
      ])
    );
  }
}
