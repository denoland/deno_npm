// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;
use std::sync::Arc;

use deno_semver::npm::NpmPackageNv;
use deno_semver::npm::NpmPackageReq;
use deno_semver::Version;
use deno_semver::VersionReq;
use futures::StreamExt;
use log::debug;
use thiserror::Error;

use super::common::NpmPackageVersionResolutionError;
use crate::registry::NpmDependencyEntry;
use crate::registry::NpmDependencyEntryError;
use crate::registry::NpmDependencyEntryKind;
use crate::registry::NpmPackageInfo;
use crate::registry::NpmPackageVersionInfo;
use crate::registry::NpmRegistryApi;
use crate::registry::NpmRegistryPackageInfoLoadError;
use crate::resolution::snapshot::SnapshotPackageCopyIndexResolver;
use crate::NpmResolutionPackageSystemInfo;

use super::common::NpmVersionResolver;
use super::common::LATEST_VERSION_REQ;
use super::snapshot::NpmResolutionSnapshot;
use crate::NpmPackageId;
use crate::NpmResolutionPackage;

// todo(dsherret): for perf we should use an arena/bump allocator for
// creating the nodes and paths since this is done in a phase

#[derive(Debug, Clone, Error)]
pub enum NpmResolutionError {
  #[error(transparent)]
  Registry(#[from] NpmRegistryPackageInfoLoadError),
  #[error(transparent)]
  Resolution(#[from] NpmPackageVersionResolutionError),
  #[error(transparent)]
  DependencyEntry(#[from] Box<NpmDependencyEntryError>),
}

/// A unique identifier to a node in the graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
struct NodeId(u32);

/// A resolved package in the resolution graph.
#[derive(Debug)]
struct Node {
  /// The specifier to child relationship in the graph. The specifier is
  /// the key in an npm package's dependencies map (ex. "express"). We
  /// use a BTreeMap for some determinism when creating the snapshot.
  ///
  /// Note: We don't want to store the children as a `NodeRef` because
  /// multiple paths might visit through the children and we don't want
  /// to share those references with those paths.
  pub children: BTreeMap<String, NodeId>,
  /// Whether the node has demonstrated to have no peer dependencies in its
  /// descendants. If this is true then we can skip analyzing this node
  /// again when we encounter it another time in the dependency tree, which
  /// is much faster.
  pub no_peers: bool,
}

#[derive(Clone)]
enum ResolvedIdPeerDep {
  /// This is a reference to the parent instead of the child because we only have a
  /// node reference to the parent, since we've traversed it, but the child node may
  /// change from under it.
  ParentReference {
    parent: GraphPathNodeOrRoot,
    child_pkg_nv: Rc<NpmPackageNv>,
  },
  /// A node that was created during snapshotting and is not being used in any path.
  SnapshotNodeId(NodeId),
}

impl ResolvedIdPeerDep {
  pub fn current_state_hash(&self) -> u64 {
    let mut hasher = DefaultHasher::new();
    self.current_state_hash_with_hasher(&mut hasher);
    hasher.finish()
  }

  pub fn current_state_hash_with_hasher(&self, hasher: &mut DefaultHasher) {
    match self {
      ResolvedIdPeerDep::ParentReference {
        parent,
        child_pkg_nv,
      } => {
        match parent {
          GraphPathNodeOrRoot::Root(root) => root.hash(hasher),
          GraphPathNodeOrRoot::Node(node) => node.node_id().hash(hasher),
        }
        child_pkg_nv.hash(hasher);
      }
      ResolvedIdPeerDep::SnapshotNodeId(node_id) => {
        node_id.hash(hasher);
      }
    }
  }
}

/// A pending resolved identifier used in the graph. At the end of resolution, these
/// will become fully resolved to an `NpmPackageId`.
#[derive(Clone)]
struct ResolvedId {
  nv: Rc<NpmPackageNv>,
  peer_dependencies: Vec<ResolvedIdPeerDep>,
}

impl ResolvedId {
  /// Gets a hash of the resolved identifier at this current moment in time.
  ///
  /// WARNING: A resolved identifier references a value that could change in
  /// the future, so this should be used with that in mind.
  pub fn current_state_hash(&self) -> u64 {
    let mut hasher = DefaultHasher::new();
    self.nv.hash(&mut hasher);
    for dep in &self.peer_dependencies {
      dep.current_state_hash_with_hasher(&mut hasher);
    }
    hasher.finish()
  }

  pub fn push_peer_dep(&mut self, peer_dep: ResolvedIdPeerDep) -> bool {
    let new_hash = peer_dep.current_state_hash();
    for dep in &self.peer_dependencies {
      if new_hash == dep.current_state_hash() {
        return false; // peer dep already set
      }
    }
    self.peer_dependencies.push(peer_dep);
    true
  }
}

/// Mappings of node identifiers to resolved identifiers. Each node has exactly
/// one resolved identifier.
///
/// The mapping from resolved to node_ids is imprecise and will do a best attempt
/// at sharing nodes.
#[derive(Default)]
struct ResolvedNodeIds {
  node_to_resolved_id: HashMap<NodeId, (ResolvedId, u64)>,
  resolved_to_node_id: HashMap<u64, NodeId>,
}

impl ResolvedNodeIds {
  pub fn set(&mut self, node_id: NodeId, resolved_id: ResolvedId) {
    let resolved_id_hash = resolved_id.current_state_hash();
    if let Some((_, old_resolved_id_key)) = self
      .node_to_resolved_id
      .insert(node_id, (resolved_id, resolved_id_hash))
    {
      // ensure the old resolved id key is removed as it might be stale
      self.resolved_to_node_id.remove(&old_resolved_id_key);
    }
    self.resolved_to_node_id.insert(resolved_id_hash, node_id);
  }

  pub fn get(&self, node_id: NodeId) -> Option<&ResolvedId> {
    self.node_to_resolved_id.get(&node_id).map(|(id, _)| id)
  }

  pub fn get_node_id(&self, resolved_id: &ResolvedId) -> Option<NodeId> {
    self
      .resolved_to_node_id
      .get(&resolved_id.current_state_hash())
      .copied()
  }
}

/// A pointer to a specific node in a graph path. The underlying node id
/// may change as peer dependencies are created.
#[derive(Clone, Debug)]
struct NodeIdRef(Rc<RefCell<NodeId>>);

impl NodeIdRef {
  pub fn new(node_id: NodeId) -> Self {
    NodeIdRef(Rc::new(RefCell::new(node_id)))
  }

  pub fn change(&self, node_id: NodeId) {
    *self.0.borrow_mut() = node_id;
  }

  pub fn get(&self) -> NodeId {
    *self.0.borrow()
  }
}

#[derive(Clone)]
enum GraphPathNodeOrRoot {
  Node(Rc<GraphPath>),
  Root(Rc<NpmPackageNv>),
}

/// Path through the graph that represents a traversal through the graph doing
/// the dependency resolution. The graph tries to share duplicate package
/// information and we try to avoid traversing parts of the graph that we know
/// are resolved.
struct GraphPath {
  previous_node: Option<GraphPathNodeOrRoot>,
  node_id_ref: NodeIdRef,
  specifier: String,
  // we could consider not storing this here and instead reference the resolved
  // nodes, but we should performance profile this code first
  nv: Rc<NpmPackageNv>,
  /// Descendants in the path that circularly link to an ancestor in a child.These
  /// descendants should be kept up to date and always point to this node.
  linked_circular_descendants: RefCell<Vec<Rc<GraphPath>>>,
}

impl GraphPath {
  pub fn for_root(node_id: NodeId, nv: Rc<NpmPackageNv>) -> Rc<Self> {
    Rc::new(Self {
      previous_node: Some(GraphPathNodeOrRoot::Root(nv.clone())),
      node_id_ref: NodeIdRef::new(node_id),
      // use an empty specifier
      specifier: "".to_string(),
      nv,
      linked_circular_descendants: Default::default(),
    })
  }

  pub fn node_id(&self) -> NodeId {
    self.node_id_ref.get()
  }

  pub fn specifier(&self) -> &str {
    &self.specifier
  }

  pub fn change_id(&self, node_id: NodeId) {
    self.node_id_ref.change(node_id)
  }

  pub fn with_id(
    self: &Rc<GraphPath>,
    node_id: NodeId,
    specifier: String,
    nv: Rc<NpmPackageNv>,
  ) -> Rc<Self> {
    Rc::new(Self {
      previous_node: Some(GraphPathNodeOrRoot::Node(self.clone())),
      node_id_ref: NodeIdRef::new(node_id),
      specifier,
      nv,
      linked_circular_descendants: Default::default(),
    })
  }

  /// Gets if there is an ancestor with the same name & version along this path.
  pub fn find_ancestor(&self, nv: &NpmPackageNv) -> Option<Rc<GraphPath>> {
    let mut maybe_next_node = self.previous_node.as_ref();
    while let Some(GraphPathNodeOrRoot::Node(next_node)) = maybe_next_node {
      // we've visited this before, so stop
      if *next_node.nv == *nv {
        return Some(next_node.clone());
      }
      maybe_next_node = next_node.previous_node.as_ref();
    }
    None
  }

  /// Gets the bottom-up path to the ancestor not including the current or ancestor node.
  pub fn get_path_to_ancestor_exclusive(
    &self,
    ancestor_node_id: NodeId,
  ) -> Vec<&Rc<GraphPath>> {
    let mut path = Vec::new();
    let mut maybe_next_node = self.previous_node.as_ref();
    while let Some(GraphPathNodeOrRoot::Node(next_node)) = maybe_next_node {
      if next_node.node_id() == ancestor_node_id {
        break;
      }
      path.push(next_node);
      maybe_next_node = next_node.previous_node.as_ref();
    }
    debug_assert!(maybe_next_node.is_some());
    path
  }

  pub fn ancestors(&self) -> GraphPathAncestorIterator {
    GraphPathAncestorIterator {
      next: self.previous_node.as_ref(),
    }
  }
}

struct GraphPathAncestorIterator<'a> {
  next: Option<&'a GraphPathNodeOrRoot>,
}

impl<'a> Iterator for GraphPathAncestorIterator<'a> {
  type Item = &'a GraphPathNodeOrRoot;
  fn next(&mut self) -> Option<Self::Item> {
    if let Some(next) = self.next.take() {
      if let GraphPathNodeOrRoot::Node(node) = next {
        self.next = node.previous_node.as_ref();
      }
      Some(next)
    } else {
      None
    }
  }
}

pub struct Graph {
  /// Each requirement is mapped to a specific name and version.
  package_reqs: HashMap<NpmPackageReq, Rc<NpmPackageNv>>,
  /// Then each name and version is mapped to an exact node id.
  /// Note: Uses a BTreeMap in order to create some determinism
  /// when creating the snapshot.
  root_packages: BTreeMap<Rc<NpmPackageNv>, NodeId>,
  package_name_versions: HashMap<String, HashSet<Version>>,
  nodes: HashMap<NodeId, Node>,
  resolved_node_ids: ResolvedNodeIds,
  // This will be set when creating from a snapshot, then
  // inform the final snapshot creation.
  packages_to_copy_index: HashMap<NpmPackageId, u8>,
  /// Packages that the resolver should resolve first.
  pending_unresolved_packages: Vec<Rc<NpmPackageNv>>,
}

impl Graph {
  pub fn from_snapshot(
    snapshot: NpmResolutionSnapshot,
  ) -> (Self, Arc<dyn NpmRegistryApi>, NpmVersionResolver) {
    fn get_or_create_graph_node(
      graph: &mut Graph,
      pkg_id: &NpmPackageId,
      packages: &HashMap<NpmPackageId, NpmResolutionPackage>,
      created_package_ids: &mut HashMap<NpmPackageId, NodeId>,
    ) -> NodeId {
      if let Some(id) = created_package_ids.get(pkg_id) {
        return *id;
      }

      let node_id = graph.create_node(&pkg_id.nv);
      created_package_ids.insert(pkg_id.clone(), node_id);

      let peer_dep_ids = pkg_id
        .peer_dependencies
        .iter()
        .map(|peer_dep| {
          ResolvedIdPeerDep::SnapshotNodeId(get_or_create_graph_node(
            graph,
            peer_dep,
            packages,
            created_package_ids,
          ))
        })
        .collect::<Vec<_>>();
      let graph_resolved_id = ResolvedId {
        nv: Rc::new(pkg_id.nv.clone()),
        peer_dependencies: peer_dep_ids,
      };
      graph.resolved_node_ids.set(node_id, graph_resolved_id);
      let resolution = match packages.get(pkg_id) {
        Some(resolved_id) => resolved_id,
        // we verify in other places that references in a snapshot
        // should be valid (ex. when creating a snapshot), so we should
        // never get here and if so that indicates a bug elsewhere
        None => panic!("not found package: {}", pkg_id.as_serialized()),
      };
      for (name, child_id) in &resolution.dependencies {
        let child_node_id = get_or_create_graph_node(
          graph,
          child_id,
          packages,
          created_package_ids,
        );
        graph.set_child_of_parent_node(node_id, name, child_node_id);
      }
      node_id
    }

    let mut graph = Self {
      // Note: It might be more correct to store the copy index
      // from past resolutions with the node somehow, but maybe not.
      packages_to_copy_index: snapshot
        .packages
        .iter()
        .map(|(id, p)| (id.clone(), p.copy_index))
        .collect(),
      package_reqs: snapshot
        .package_reqs
        .into_iter()
        .map(|(k, v)| (k, Rc::new(v)))
        .collect(),
      pending_unresolved_packages: snapshot
        .pending_unresolved_packages
        .into_iter()
        .map(Rc::new)
        .collect(),
      nodes: Default::default(),
      package_name_versions: Default::default(),
      resolved_node_ids: Default::default(),
      root_packages: Default::default(),
    };
    let mut created_package_ids =
      HashMap::with_capacity(snapshot.packages.len());
    for (id, resolved_id) in snapshot.root_packages {
      let node_id = get_or_create_graph_node(
        &mut graph,
        &resolved_id,
        &snapshot.packages,
        &mut created_package_ids,
      );
      graph.root_packages.insert(Rc::new(id), node_id);
    }
    (graph, snapshot.api, snapshot.version_resolver)
  }

  pub fn take_pending_unresolved(&mut self) -> Vec<Rc<NpmPackageNv>> {
    std::mem::take(&mut self.pending_unresolved_packages)
  }

  pub fn has_package_req(&self, req: &NpmPackageReq) -> bool {
    self.package_reqs.contains_key(req)
  }

  pub fn has_root_package(&self, id: &NpmPackageNv) -> bool {
    self.root_packages.contains_key(id)
  }

  fn get_npm_pkg_id(&self, node_id: NodeId) -> NpmPackageId {
    let resolved_id = self.resolved_node_ids.get(node_id).unwrap();
    self.get_npm_pkg_id_from_resolved_id(resolved_id, HashSet::new())
  }

  fn get_npm_pkg_id_from_resolved_id(
    &self,
    resolved_id: &ResolvedId,
    seen: HashSet<NodeId>,
  ) -> NpmPackageId {
    if resolved_id.peer_dependencies.is_empty() {
      NpmPackageId {
        nv: (*resolved_id.nv).clone(),
        peer_dependencies: Vec::new(),
      }
    } else {
      let mut npm_pkg_id = NpmPackageId {
        nv: (*resolved_id.nv).clone(),
        peer_dependencies: Vec::with_capacity(
          resolved_id.peer_dependencies.len(),
        ),
      };
      let mut seen_children_resolved_ids =
        HashSet::with_capacity(resolved_id.peer_dependencies.len());
      for peer_dep in &resolved_id.peer_dependencies {
        let maybe_node_and_resolved_id = match peer_dep {
          ResolvedIdPeerDep::SnapshotNodeId(node_id) => self
            .resolved_node_ids
            .get(*node_id)
            .map(|resolved_id| (*node_id, resolved_id)),
          ResolvedIdPeerDep::ParentReference {
            parent,
            child_pkg_nv: child_nv,
          } => match &parent {
            GraphPathNodeOrRoot::Root(_) => {
              self.root_packages.get(child_nv).and_then(|node_id| {
                self
                  .resolved_node_ids
                  .get(*node_id)
                  .map(|resolved_id| (*node_id, resolved_id))
              })
            }
            GraphPathNodeOrRoot::Node(parent_path) => {
              self.nodes.get(&parent_path.node_id()).and_then(|parent| {
                parent
                  .children
                  .values()
                  .filter_map(|child_id| {
                    let child_id = *child_id;
                    self
                      .resolved_node_ids
                      .get(child_id)
                      .map(|resolved_id| (child_id, resolved_id))
                  })
                  .find(|(_, resolved_id)| resolved_id.nv == *child_nv)
              })
            }
          },
        };
        // this should always be set
        debug_assert!(maybe_node_and_resolved_id.is_some());
        if let Some((child_id, child_resolved_id)) = maybe_node_and_resolved_id
        {
          let mut new_seen = seen.clone();
          if new_seen.insert(child_id) {
            let child_peer = self.get_npm_pkg_id_from_resolved_id(
              child_resolved_id,
              new_seen.clone(),
            );

            if seen_children_resolved_ids.insert(child_peer.clone()) {
              npm_pkg_id.peer_dependencies.push(child_peer);
            }
          }
        }
      }
      npm_pkg_id
    }
  }

  fn get_or_create_for_id(
    &mut self,
    resolved_id: &ResolvedId,
  ) -> (bool, NodeId) {
    if let Some(node_id) = self.resolved_node_ids.get_node_id(resolved_id) {
      return (false, node_id);
    }

    let node_id = self.create_node(&resolved_id.nv);
    self.resolved_node_ids.set(node_id, resolved_id.clone());
    (true, node_id)
  }

  fn create_node(&mut self, pkg_nv: &NpmPackageNv) -> NodeId {
    let node_id = NodeId(self.nodes.len() as u32);
    let node = Node {
      children: Default::default(),
      no_peers: false,
    };

    self
      .package_name_versions
      .entry(pkg_nv.name.clone())
      .or_default()
      .insert(pkg_nv.version.clone());
    self.nodes.insert(node_id, node);

    node_id
  }

  fn borrow_node_mut(&mut self, node_id: NodeId) -> &mut Node {
    self.nodes.get_mut(&node_id).unwrap()
  }

  fn set_child_of_parent_node(
    &mut self,
    parent_id: NodeId,
    specifier: &str,
    child_id: NodeId,
  ) {
    assert_ne!(child_id, parent_id);
    let parent = self.borrow_node_mut(parent_id);
    parent.children.insert(specifier.to_string(), child_id);
  }

  pub async fn into_snapshot(
    self,
    api: Arc<dyn NpmRegistryApi>,
    version_resolver: NpmVersionResolver,
  ) -> Result<NpmResolutionSnapshot, NpmRegistryPackageInfoLoadError> {
    let packages_to_pkg_ids = self
      .nodes
      .keys()
      .map(|node_id| (*node_id, self.get_npm_pkg_id(*node_id)))
      .collect::<HashMap<_, _>>();
    let mut copy_index_resolver =
      SnapshotPackageCopyIndexResolver::from_map_with_capacity(
        self.packages_to_copy_index,
        self.nodes.len(),
      );
    let mut packages: HashMap<NpmPackageId, NpmResolutionPackage> =
      HashMap::with_capacity(self.nodes.len());
    let mut packages_by_name: HashMap<String, Vec<_>> =
      HashMap::with_capacity(self.nodes.len());

    // todo(dsherret): there is a lurking bug within the peer dependencies code.
    // You can see it by using `NodeIds` instead of `NpmPackageIds` on this travered_ids
    // hashset, which will cause the bottom of the "tree" nodes to be populated in
    // the result instead of the top of the "tree". I think there's maybe one small
    // thing that's not being updated properly.
    let mut traversed_ids = HashSet::with_capacity(self.nodes.len());
    let mut pending = VecDeque::new();

    for root_id in self.root_packages.values().copied() {
      let pkg_id = packages_to_pkg_ids.get(&root_id).unwrap();
      if traversed_ids.insert(pkg_id.clone()) {
        pending.push_back((root_id, pkg_id));
      }
    }

    while let Some((node_id, pkg_id)) = pending.pop_front() {
      let node = self.nodes.get(&node_id).unwrap();

      packages_by_name
        .entry(pkg_id.nv.name.clone())
        .or_default()
        .push(pkg_id.clone());

      // at this point the api should have this cached
      let package_info = api.package_info(&pkg_id.nv.name).await?;
      let version_info = package_info
        .versions
        .get(&pkg_id.nv.version)
        .unwrap_or_else(|| panic!("missing: {:?}", pkg_id.nv));

      let mut dependencies = HashMap::with_capacity(node.children.len());
      for (specifier, child_id) in &node.children {
        let child_id = *child_id;
        let child_pkg_id = packages_to_pkg_ids.get(&child_id).unwrap();
        if traversed_ids.insert(child_pkg_id.clone()) {
          pending.push_back((child_id, child_pkg_id));
        }
        dependencies.insert(specifier.clone(), (*child_pkg_id).clone());
      }

      packages.insert(
        (*pkg_id).clone(),
        NpmResolutionPackage {
          copy_index: copy_index_resolver.resolve(pkg_id),
          id: (*pkg_id).clone(),
          system: NpmResolutionPackageSystemInfo {
            cpu: version_info.cpu.clone(),
            os: version_info.os.clone(),
          },
          dist: version_info.dist.clone(),
          dependencies,
          optional_dependencies: version_info
            .optional_dependencies
            .keys()
            .cloned()
            .collect(),
        },
      );
    }

    Ok(NpmResolutionSnapshot {
      api,
      version_resolver,
      root_packages: self
        .root_packages
        .into_iter()
        .map(|(nv, node_id)| {
          (
            (*nv).clone(),
            packages_to_pkg_ids.get(&node_id).unwrap().clone(),
          )
        })
        .collect(),
      packages_by_name: packages_by_name
        .into_iter()
        .map(|(name, mut ids)| {
          ids.sort();
          ids.dedup();
          (name, ids)
        })
        .collect(),
      packages,
      package_reqs: self
        .package_reqs
        .into_iter()
        .map(|(req, nv)| (req, (*nv).clone()))
        .collect(),
      pending_unresolved_packages: self
        .pending_unresolved_packages
        .into_iter()
        .map(|nv| (*nv).clone())
        .collect(),
    })
  }

  // Debugging methods

  #[cfg(debug_assertions)]
  #[allow(unused)]
  fn output_path(&self, path: &Rc<GraphPath>) {
    eprintln!("-----------");
    self.output_node(path.node_id(), false);
    for path in path.ancestors() {
      match path {
        GraphPathNodeOrRoot::Node(node) => {
          self.output_node(node.node_id(), false)
        }
        GraphPathNodeOrRoot::Root(pkg_id) => {
          let node_id = self.root_packages.get(pkg_id).unwrap();
          eprintln!(
            "Root: {} ({}: {})",
            pkg_id,
            node_id.0,
            self.get_npm_pkg_id(*node_id).as_serialized()
          )
        }
      }
    }
    eprintln!("-----------");
  }

  #[cfg(debug_assertions)]
  #[allow(unused)]
  fn output_node(&self, node_id: NodeId, show_children: bool) {
    eprintln!(
      "{:>4}: {}",
      node_id.0,
      self.get_npm_pkg_id(node_id).as_serialized()
    );

    if show_children {
      let node = self.nodes.get(&node_id).unwrap();
      eprintln!("       Children:");
      for (specifier, child_id) in &node.children {
        eprintln!("         {}: {}", specifier, child_id.0);
      }
    }
  }

  #[cfg(debug_assertions)]
  #[allow(unused)]
  pub fn output_nodes(&self) {
    eprintln!("~~~");
    let mut node_ids = self
      .resolved_node_ids
      .node_to_resolved_id
      .keys()
      .copied()
      .collect::<Vec<_>>();
    node_ids.sort_by(|a, b| a.0.cmp(&b.0));
    for node_id in node_ids {
      self.output_node(node_id, true);
    }
    eprintln!("~~~");
  }
}

#[derive(Default)]
struct DepEntryCache(HashMap<Rc<NpmPackageNv>, Rc<Vec<NpmDependencyEntry>>>);

impl DepEntryCache {
  pub fn store(
    &mut self,
    nv: Rc<NpmPackageNv>,
    version_info: &NpmPackageVersionInfo,
  ) -> Result<Rc<Vec<NpmDependencyEntry>>, Box<NpmDependencyEntryError>> {
    debug_assert_eq!(nv.version, version_info.version);
    debug_assert!(!self.0.contains_key(&nv)); // we should not be re-inserting
    let mut deps = version_info.dependencies_as_entries(&nv.name)?;
    // Ensure name alphabetical and then version descending
    // so these are resolved in that order
    deps.sort();
    let deps = Rc::new(deps);
    self.0.insert(nv, deps.clone());
    Ok(deps)
  }

  pub fn get(&self, id: &NpmPackageNv) -> Option<&Rc<Vec<NpmDependencyEntry>>> {
    self.0.get(id)
  }
}

struct UnresolvedOptionalPeer {
  specifier: String,
  graph_path: Rc<GraphPath>,
}

pub struct GraphDependencyResolver<'a> {
  graph: &'a mut Graph,
  api: &'a dyn NpmRegistryApi,
  version_resolver: &'a NpmVersionResolver,
  pending_unresolved_nodes: VecDeque<Rc<GraphPath>>,
  unresolved_optional_peers:
    HashMap<Rc<NpmPackageNv>, Vec<UnresolvedOptionalPeer>>,
  dep_entry_cache: DepEntryCache,
}

impl<'a> GraphDependencyResolver<'a> {
  pub fn new(
    graph: &'a mut Graph,
    api: &'a dyn NpmRegistryApi,
    version_resolver: &'a NpmVersionResolver,
  ) -> Self {
    Self {
      graph,
      api,
      version_resolver,
      pending_unresolved_nodes: Default::default(),
      unresolved_optional_peers: Default::default(),
      dep_entry_cache: Default::default(),
    }
  }

  pub fn add_root_package(
    &mut self,
    package_nv: &NpmPackageNv,
    package_info: &NpmPackageInfo,
  ) -> Result<(), NpmResolutionError> {
    if self.graph.root_packages.contains_key(package_nv) {
      return Ok(()); // already added
    }

    // todo(dsherret): using a version requirement here is a temporary hack
    // to reuse code in a large refactor. We should resolve the node directly
    // from the package name and version
    let version_req =
      VersionReq::parse_from_specifier(&format!("{}", package_nv.version))
        .unwrap();
    let (pkg_nv, node_id) = self.resolve_node_from_info(
      &package_nv.name,
      &version_req,
      package_info,
      None,
    )?;
    self.graph.root_packages.insert(pkg_nv.clone(), node_id);
    self
      .pending_unresolved_nodes
      .push_back(GraphPath::for_root(node_id, pkg_nv));
    Ok(())
  }

  pub fn add_package_req(
    &mut self,
    package_req: &NpmPackageReq,
    package_info: &NpmPackageInfo,
  ) -> Result<(), NpmResolutionError> {
    if self.graph.package_reqs.contains_key(package_req) {
      return Ok(()); // already added
    }

    let (pkg_id, node_id) = self.resolve_node_from_info(
      &package_req.name,
      package_req
        .version_req
        .as_ref()
        .unwrap_or(&*LATEST_VERSION_REQ),
      package_info,
      None,
    )?;
    self
      .graph
      .package_reqs
      .insert(package_req.clone(), pkg_id.clone());
    self.graph.root_packages.insert(pkg_id.clone(), node_id);
    self
      .pending_unresolved_nodes
      .push_back(GraphPath::for_root(node_id, pkg_id));
    Ok(())
  }

  fn analyze_dependency(
    &mut self,
    entry: &NpmDependencyEntry,
    package_info: &NpmPackageInfo,
    parent_path: &Rc<GraphPath>,
  ) -> Result<NodeId, NpmResolutionError> {
    debug_assert_eq!(entry.kind, NpmDependencyEntryKind::Dep);
    let parent_id = parent_path.node_id();
    let (child_nv, mut child_id) = self.resolve_node_from_info(
      &entry.name,
      &entry.version_req,
      package_info,
      Some(parent_id),
    )?;
    // Some packages may resolves to themselves as a dependency. If this occurs,
    // just ignore adding these as dependencies because this is likely a mistake
    // in the package.
    if child_id != parent_id {
      let maybe_ancestor = parent_path.find_ancestor(&child_nv);
      if let Some(ancestor) = &maybe_ancestor {
        child_id = ancestor.node_id();
      }

      let new_path = parent_path.with_id(
        child_id,
        entry.bare_specifier.to_string(),
        child_nv,
      );
      if let Some(ancestor) = maybe_ancestor {
        // this node is circular, so we link it to the ancestor
        self.add_linked_circular_descendant(&ancestor, new_path);
      } else {
        self.graph.set_child_of_parent_node(
          parent_id,
          &entry.bare_specifier,
          child_id,
        );
        self.pending_unresolved_nodes.push_back(new_path);
      }
    }
    Ok(child_id)
  }

  fn resolve_node_from_info(
    &mut self,
    pkg_req_name: &str,
    version_req: &VersionReq,
    package_info: &NpmPackageInfo,
    parent_id: Option<NodeId>,
  ) -> Result<(Rc<NpmPackageNv>, NodeId), NpmResolutionError> {
    let info = self.version_resolver.resolve_best_package_version_info(
      version_req,
      package_info,
      self
        .graph
        .package_name_versions
        .entry(package_info.name.clone())
        .or_default()
        .iter(),
    )?;
    let resolved_id = ResolvedId {
      nv: Rc::new(NpmPackageNv {
        name: package_info.name.to_string(),
        version: info.version.clone(),
      }),
      peer_dependencies: Vec::new(),
    };
    let (_, node_id) = self.graph.get_or_create_for_id(&resolved_id);
    let pkg_nv = resolved_id.nv;

    let has_deps = if let Some(deps) = self.dep_entry_cache.get(&pkg_nv) {
      !deps.is_empty()
    } else {
      let deps = self.dep_entry_cache.store(pkg_nv.clone(), info)?;
      !deps.is_empty()
    };

    if !has_deps {
      // ensure this is set if not, as it's an optimization
      let mut node = self.graph.borrow_node_mut(node_id);
      node.no_peers = true;
    }

    debug!(
      "{} - Resolved {}@{} to {}",
      match parent_id {
        Some(parent_id) => self.graph.get_npm_pkg_id(parent_id).as_serialized(),
        None => "<package-req>".to_string(),
      },
      pkg_req_name,
      version_req.version_text(),
      pkg_nv.to_string(),
    );

    Ok((pkg_nv, node_id))
  }

  pub async fn resolve_pending(&mut self) -> Result<(), NpmResolutionError> {
    // go down through the dependencies by tree depth
    while let Some(parent_path) = self.pending_unresolved_nodes.pop_front() {
      let (parent_nv, child_deps) = {
        let node_id = parent_path.node_id();
        if self.graph.nodes.get(&node_id).unwrap().no_peers {
          // We can skip as there's no reason to analyze this graph segment further.
          continue;
        }

        let pkg_nv = self
          .graph
          .resolved_node_ids
          .get(node_id)
          .unwrap()
          .nv
          .clone();
        let deps = if let Some(deps) = self.dep_entry_cache.get(&pkg_nv) {
          deps.clone()
        } else {
          // the api is expected to have cached this at this point, so no
          // need to parallelize
          let package_info = self.api.package_info(&pkg_nv.name).await?;
          let version_info = package_info
            .version_info(&pkg_nv)
            .map_err(NpmPackageVersionResolutionError::VersionNotFound)?;
          self.dep_entry_cache.store(pkg_nv.clone(), &version_info)?
        };

        (pkg_nv, deps)
      };

      // resolve the dependencies
      let mut found_peer = false;

      let mut infos = futures::stream::FuturesOrdered::from_iter(
        child_deps
          .iter()
          .map(|dep| self.api.package_info(&dep.name)),
      );

      let mut child_deps_iter = child_deps.iter();
      while let Some(package_info) = infos.next().await {
        let package_info = package_info?;
        let dep = child_deps_iter.next().unwrap();

        match dep.kind {
          NpmDependencyEntryKind::Dep => {
            let parent_id = parent_path.node_id();
            let node = self.graph.nodes.get(&parent_id).unwrap();
            let child_id = match node.children.get(&dep.bare_specifier) {
              Some(child_id) => {
                // this dependency was previously analyzed by another path
                // so we don't attempt to resolve the version again
                let child_id = *child_id;
                let child_nv = self
                  .graph
                  .resolved_node_ids
                  .get(child_id)
                  .unwrap()
                  .nv
                  .clone();
                let maybe_ancestor = parent_path.find_ancestor(&child_nv);
                let child_path = parent_path.with_id(
                  child_id,
                  dep.bare_specifier.clone(),
                  child_nv,
                );
                if let Some(ancestor) = maybe_ancestor {
                  // when the nv appears as an ancestor, use that node
                  // and mark this as circular
                  self.add_linked_circular_descendant(&ancestor, child_path);
                } else {
                  // mark the child as pending
                  self.pending_unresolved_nodes.push_back(child_path);
                }
                child_id
              }
              None => {
                self.analyze_dependency(dep, &package_info, &parent_path)?
              }
            };

            if !found_peer {
              found_peer = !self.graph.borrow_node_mut(child_id).no_peers;
            }
          }
          NpmDependencyEntryKind::Peer
          | NpmDependencyEntryKind::OptionalPeer => {
            found_peer = true;
            // we need to re-evaluate peer dependencies every time and can't
            // skip over them because they might be evaluated differently based
            // on the current path
            let maybe_new_id = self.resolve_peer_dep(
              &dep.bare_specifier,
              dep,
              &package_info,
              &parent_path,
            )?;

            // For optional peer dependencies, we want to resolve them if any future
            // same parent version resolves them. So when not resolved, store them to be
            // potentially resolved later.
            //
            // Note: This is not a good solution, but will probably work ok in most
            // scenarios. We can work on improving this in the future. We probably
            // want to resolve future optional peers to the same dependency for example.
            if dep.kind == NpmDependencyEntryKind::OptionalPeer {
              match maybe_new_id {
                Some(new_id) => {
                  if let Some(unresolved_optional_peers) =
                    self.unresolved_optional_peers.get_mut(&parent_nv)
                  {
                    // todo(dsherret): use drain_retain once it's not in nightly rust
                    let mut peers =
                      VecDeque::with_capacity(unresolved_optional_peers.len());
                    for i in (0..unresolved_optional_peers.len()).rev() {
                      if unresolved_optional_peers[i].specifier
                        == dep.bare_specifier
                      {
                        peers.push_front(unresolved_optional_peers.remove(i));
                      }
                    }

                    for optional_peer in peers {
                      let peer_parent = GraphPathNodeOrRoot::Node(
                        optional_peer.graph_path.clone(),
                      );
                      self.set_new_peer_dep(
                        &[&optional_peer.graph_path],
                        peer_parent,
                        &optional_peer.specifier,
                        new_id,
                      );
                    }
                  }
                }
                None => {
                  // store this for later if it's resolved for this version
                  self
                    .unresolved_optional_peers
                    .entry(parent_nv.clone())
                    .or_default()
                    .push(UnresolvedOptionalPeer {
                      specifier: dep.bare_specifier.clone(),
                      graph_path: parent_path.clone(),
                    });
                }
              }
            }
          }
        }
      }

      if !found_peer {
        self.graph.borrow_node_mut(parent_path.node_id()).no_peers = true;
      }
    }
    Ok(())
  }

  fn resolve_peer_dep(
    &mut self,
    specifier: &str,
    peer_dep: &NpmDependencyEntry,
    peer_package_info: &NpmPackageInfo,
    ancestor_path: &Rc<GraphPath>,
  ) -> Result<Option<NodeId>, NpmResolutionError> {
    debug_assert!(matches!(
      peer_dep.kind,
      NpmDependencyEntryKind::Peer | NpmDependencyEntryKind::OptionalPeer
    ));

    let mut path = vec![ancestor_path];

    // the current dependency might have had the peer dependency
    // in another bare specifier slot... if so resolve it to that
    {
      let maybe_peer_dep = self.find_peer_dep_in_node(
        ancestor_path,
        peer_dep,
        peer_package_info,
      )?;

      if let Some((peer_parent, peer_dep_id)) = maybe_peer_dep {
        // this will always have an ancestor because we're not at the root
        self.set_new_peer_dep(&path, peer_parent, specifier, peer_dep_id);
        return Ok(Some(peer_dep_id));
      }
    }

    // Peer dependencies are resolved based on its ancestors' siblings.
    // If not found, then it resolves based on the version requirement if non-optional.
    for ancestor_node in ancestor_path.ancestors() {
      match ancestor_node {
        GraphPathNodeOrRoot::Node(ancestor_graph_path_node) => {
          path.push(ancestor_graph_path_node);
          let maybe_peer_dep = self.find_peer_dep_in_node(
            ancestor_graph_path_node,
            peer_dep,
            peer_package_info,
          )?;
          if let Some((parent, peer_dep_id)) = maybe_peer_dep {
            // this will always have an ancestor because we're not at the root
            self.set_new_peer_dep(&path, parent, specifier, peer_dep_id);
            return Ok(Some(peer_dep_id));
          }
        }
        GraphPathNodeOrRoot::Root(root_pkg_id) => {
          // in this case, the parent is the root so the children are all the package requirements
          if let Some(child_id) = self.find_matching_child(
            peer_dep,
            peer_package_info,
            self.graph.root_packages.iter().map(|(nv, id)| (*id, nv)),
          )? {
            let peer_parent = GraphPathNodeOrRoot::Root(root_pkg_id.clone());
            self.set_new_peer_dep(&path, peer_parent, specifier, child_id);
            return Ok(Some(child_id));
          }
        }
      }
    }

    // We didn't find anything by searching the ancestor siblings, so we need
    // to resolve based on the package info
    if !peer_dep.kind.is_optional() {
      let parent_id = ancestor_path.node_id();
      let (_, node_id) = self.resolve_node_from_info(
        &peer_dep.name,
        peer_dep
          .peer_dep_version_req
          .as_ref()
          .unwrap_or(&peer_dep.version_req),
        peer_package_info,
        Some(parent_id),
      )?;
      let peer_parent = GraphPathNodeOrRoot::Node(ancestor_path.clone());
      self.set_new_peer_dep(&[ancestor_path], peer_parent, specifier, node_id);
      Ok(Some(node_id))
    } else {
      Ok(None)
    }
  }

  fn find_peer_dep_in_node(
    &self,
    path: &Rc<GraphPath>,
    peer_dep: &NpmDependencyEntry,
    peer_package_info: &NpmPackageInfo,
  ) -> Result<Option<(GraphPathNodeOrRoot, NodeId)>, NpmResolutionError> {
    let node_id = path.node_id();
    let resolved_node_id = self.graph.resolved_node_ids.get(node_id).unwrap();
    // check if this node itself is a match for
    // the peer dependency and if so use that
    if resolved_node_id.nv.name == peer_dep.name
      && self.version_resolver.version_req_satisfies(
        &peer_dep.version_req,
        &resolved_node_id.nv.version,
        peer_package_info,
      )?
    {
      let parent = path.previous_node.as_ref().unwrap().clone();
      Ok(Some((parent, node_id)))
    } else {
      let node = self.graph.nodes.get(&node_id).unwrap();
      let children = node.children.values().map(|child_node_id| {
        let child_node_id = *child_node_id;
        (
          child_node_id,
          &self.graph.resolved_node_ids.get(child_node_id).unwrap().nv,
        )
      });
      self
        .find_matching_child(peer_dep, peer_package_info, children)
        .map(|maybe_child_id| {
          maybe_child_id.map(|child_id| {
            let parent = GraphPathNodeOrRoot::Node(path.clone());
            (parent, child_id)
          })
        })
    }
  }

  fn add_peer_deps_to_path(
    &mut self,
    // path from the node above the resolved dep to just above the peer dep
    path: &[&Rc<GraphPath>],
    peer_deps: &[(&ResolvedIdPeerDep, Rc<NpmPackageNv>)],
  ) {
    debug_assert!(!path.is_empty());

    for graph_path_node in path.iter().rev() {
      let old_node_id = graph_path_node.node_id();
      let old_resolved_id = self
        .graph
        .resolved_node_ids
        .get(old_node_id)
        .unwrap()
        .clone();

      let mut new_resolved_id = old_resolved_id;
      let mut has_changed = false;
      for (peer_dep, nv) in peer_deps {
        if *nv == new_resolved_id.nv {
          continue;
        }
        if new_resolved_id.push_peer_dep((*peer_dep).clone()) {
          has_changed = true;
        }
      }

      if !has_changed {
        continue; // nothing to change
      }

      let (created, new_node_id) =
        self.graph.get_or_create_for_id(&new_resolved_id);

      if created {
        let old_children =
          self.graph.borrow_node_mut(old_node_id).children.clone();
        // copy over the old children to this new one
        for (specifier, child_id) in &old_children {
          self.graph.set_child_of_parent_node(
            new_node_id,
            specifier,
            *child_id,
          );
        }
      }

      graph_path_node.change_id(new_node_id);

      let circular_descendants =
        graph_path_node.linked_circular_descendants.borrow().clone();
      for descendant in circular_descendants {
        let path = descendant.get_path_to_ancestor_exclusive(new_node_id);
        self.add_peer_deps_to_path(&path, peer_deps);
        descendant.change_id(new_node_id);

        // update the bottom node to point to this new node id
        let bottom_node_id = path[0].node_id();
        self.graph.set_child_of_parent_node(
          bottom_node_id,
          descendant.specifier(),
          descendant.node_id(),
        );
      }

      // update the previous parent to have this as its child
      match graph_path_node.previous_node.as_ref().unwrap() {
        GraphPathNodeOrRoot::Root(pkg_id) => {
          self.graph.root_packages.insert(pkg_id.clone(), new_node_id);
        }
        GraphPathNodeOrRoot::Node(parent_node_path) => {
          let parent_node_id = parent_node_path.node_id();
          let parent_node = self.graph.borrow_node_mut(parent_node_id);
          parent_node
            .children
            .insert(graph_path_node.specifier().to_string(), new_node_id);
        }
      }
    }
  }

  fn set_new_peer_dep(
    &mut self,
    // path from the node above the resolved dep to just above the peer dep
    path: &[&Rc<GraphPath>],
    peer_dep_parent: GraphPathNodeOrRoot,
    peer_dep_specifier: &str,
    peer_dep_id: NodeId,
  ) {
    debug_assert!(!path.is_empty());
    let peer_dep_nv = self
      .graph
      .resolved_node_ids
      .get(peer_dep_id)
      .unwrap()
      .nv
      .clone();

    let peer_dep = ResolvedIdPeerDep::ParentReference {
      parent: peer_dep_parent,
      child_pkg_nv: peer_dep_nv.clone(),
    };

    let top_node = path.last().unwrap();
    let (maybe_circular_ancestor, path) = if top_node.nv == peer_dep_nv {
      // it's circular, so exclude the top node
      (Some(top_node), &path[0..path.len() - 1])
    } else {
      (None, path)
    };
    self.add_peer_deps_to_path(path, &[(&peer_dep, peer_dep_nv.clone())]);

    // now set the peer dependency
    let bottom_node = path.first().unwrap();
    self.graph.set_child_of_parent_node(
      bottom_node.node_id(),
      peer_dep_specifier,
      peer_dep_id,
    );

    // queue next step
    let new_path = bottom_node.with_id(
      peer_dep_id,
      peer_dep_specifier.to_string(),
      peer_dep_nv,
    );
    if let Some(ancestor_node) = maybe_circular_ancestor {
      // it's circular, so link this in step with the ancestor node
      ancestor_node
        .linked_circular_descendants
        .borrow_mut()
        .push(new_path);
    } else {
      // mark the peer dep as needing to be analyzed
      self.pending_unresolved_nodes.push_back(new_path);
    }

    debug!(
      "Resolved peer dependency for {} in {} to {}",
      peer_dep_specifier,
      &self
        .graph
        .get_npm_pkg_id(bottom_node.node_id())
        .as_serialized(),
      &self.graph.get_npm_pkg_id(peer_dep_id).as_serialized(),
    );
  }

  fn add_linked_circular_descendant(
    &mut self,
    ancestor: &Rc<GraphPath>,
    descendant: Rc<GraphPath>,
  ) {
    let ancestor_node_id = ancestor.node_id();
    let path = descendant.get_path_to_ancestor_exclusive(ancestor_node_id);

    let ancestor_resolved_id = self
      .graph
      .resolved_node_ids
      .get(ancestor_node_id)
      .unwrap()
      .clone();

    let peer_deps = ancestor_resolved_id
      .peer_dependencies
      .iter()
      .map(|peer_dep| {
        (
          peer_dep,
          match &peer_dep {
            ResolvedIdPeerDep::ParentReference { child_pkg_nv, .. } => {
              child_pkg_nv.clone()
            }
            ResolvedIdPeerDep::SnapshotNodeId(node_id) => self
              .graph
              .resolved_node_ids
              .get(*node_id)
              .unwrap()
              .nv
              .clone(),
          },
        )
      })
      .collect::<Vec<_>>();
    if !peer_deps.is_empty() {
      self.add_peer_deps_to_path(&path, &peer_deps);
    }

    let bottom_node_id = path[0].node_id();
    self.graph.set_child_of_parent_node(
      bottom_node_id,
      descendant.specifier(),
      descendant.node_id(),
    );

    ancestor
      .linked_circular_descendants
      .borrow_mut()
      .push(descendant);
  }

  fn find_matching_child<'nv>(
    &self,
    peer_dep: &NpmDependencyEntry,
    peer_package_info: &NpmPackageInfo,
    children: impl Iterator<Item = (NodeId, &'nv Rc<NpmPackageNv>)>,
  ) -> Result<Option<NodeId>, NpmResolutionError> {
    for (child_id, pkg_id) in children {
      if pkg_id.name == peer_dep.name
        && self.version_resolver.version_req_satisfies(
          &peer_dep.version_req,
          &pkg_id.version,
          peer_package_info,
        )?
      {
        return Ok(Some(child_id));
      }
    }
    Ok(None)
  }
}

#[cfg(test)]
mod test {
  use deno_semver::npm::NpmPackageReqReference;
  use pretty_assertions::assert_eq;

  use crate::registry::TestNpmRegistryApi;
  use crate::resolution::NpmResolutionSnapshotCreateOptions;
  use crate::resolution::SerializedNpmResolutionSnapshot;
  use crate::NpmSystemInfo;

  use super::*;

  #[test]
  fn resolved_id_tests() {
    let mut ids = ResolvedNodeIds::default();
    let node_id = NodeId(0);
    let resolved_id = ResolvedId {
      nv: Rc::new(NpmPackageNv::from_str("package@1.1.1").unwrap()),
      peer_dependencies: Vec::new(),
    };
    ids.set(node_id, resolved_id.clone());
    assert!(ids.get(node_id).is_some());
    assert!(ids.get(NodeId(1)).is_none());
    assert_eq!(ids.get_node_id(&resolved_id), Some(node_id));

    let resolved_id_new = ResolvedId {
      nv: Rc::new(NpmPackageNv::from_str("package@1.1.2").unwrap()),
      peer_dependencies: Vec::new(),
    };
    ids.set(node_id, resolved_id_new.clone());
    assert_eq!(ids.get_node_id(&resolved_id), None); // stale entry should have been removed
    assert!(ids.get(node_id).is_some());
    assert_eq!(ids.get_node_id(&resolved_id_new), Some(node_id));
  }

  #[tokio::test]
  async fn resolve_deps_no_peer() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.ensure_package_version("package-c", "0.1.0");
    api.ensure_package_version("package-c", "0.0.10");
    api.ensure_package_version("package-d", "3.2.1");
    api.ensure_package_version("package-d", "3.2.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "^2"));
    api.add_dependency(("package-a", "1.0.0"), ("package-c", "^0.1"));
    api.add_dependency(("package-c", "0.1.0"), ("package-d", "*"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            ("package-b".to_string(), "package-b@2.0.0".to_string(),),
            ("package-c".to_string(), "package-c@0.1.0".to_string(),),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@0.1.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-d".to_string(),
            "package-d@3.2.1".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-d@3.2.1".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn resolve_deps_circular() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "*"));
    api.add_dependency(("package-b", "2.0.0"), ("package-a", "1"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@2.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0".to_string(),
          )]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1.0".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn peer_deps_simple_top_tree() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-peer", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_peer_dependency(("package-b", "1.0.0"), ("package-peer", "*"));

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec!["npm:package-a@1.0", "npm:package-peer@1.0"],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0_package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        }
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-a@1.0".to_string(),
          "package-a@1.0.0_package-peer@1.0.0".to_string()
        ),
        (
          "package-peer@1.0".to_string(),
          "package-peer@1.0.0".to_string()
        )
      ]
    );
  }

  #[tokio::test]
  async fn peer_deps_simple_root_pkg_children() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-0", "1.0.0");
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-peer", "1.0.0");
    api.add_dependency(("package-0", "1.0.0"), ("package-a", "1"));
    api.add_dependency(("package-0", "1.0.0"), ("package-peer", "1"));
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_peer_dependency(("package-b", "1.0.0"), ("package-peer", "*"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-0@1.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-0@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-a".to_string(),
              "package-a@1.0.0_package-peer@1.0.0".to_string(),
            ),
            ("package-peer".to_string(), "package-peer@1.0.0".to_string(),)
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0_package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        }
      ]
    );
    assert_eq!(
      package_reqs,
      vec![(
        "package-0@1.0".to_string(),
        "package-0@1.0.0_package-peer@1.0.0".to_string()
      ),]
    );
  }

  #[tokio::test]
  async fn peer_deps_simple_deeper() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-0", "1.0.0");
    api.ensure_package_version("package-1", "1.0.0");
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-peer", "1.0.0");
    api.add_dependency(("package-0", "1.0.0"), ("package-1", "1"));
    api.add_dependency(("package-1", "1.0.0"), ("package-a", "1"));
    api.add_dependency(("package-1", "1.0.0"), ("package-peer", "1"));
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_peer_dependency(("package-b", "1.0.0"), ("package-peer", "*"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-0@1.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-0@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-1".to_string(),
            "package-1@1.0.0_package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-1@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-a".to_string(),
              "package-a@1.0.0_package-peer@1.0.0".to_string(),
            ),
            ("package-peer".to_string(), "package-peer@1.0.0".to_string(),)
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0_package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        }
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-0@1.0".to_string(), "package-0@1.0.0".to_string()),]
    );
  }

  #[tokio::test]
  async fn resolve_with_peer_deps_top_tree() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.ensure_package_version("package-c", "3.0.0");
    api.ensure_package_version("package-peer", "4.0.0");
    api.ensure_package_version("package-peer", "4.1.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "^2"));
    api.add_dependency(("package-a", "1.0.0"), ("package-c", "^3"));
    api.add_peer_dependency(("package-b", "2.0.0"), ("package-peer", "4"));
    api.add_peer_dependency(("package-c", "3.0.0"), ("package-peer", "*"));

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      // the peer dependency is specified here at the top of the tree
      // so it should resolve to 4.0.0 instead of 4.1.0
      vec!["npm:package-a@1", "npm:package-peer@4.0.0"],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-b".to_string(),
              "package-b@2.0.0_package-peer@4.0.0".to_string(),
            ),
            (
              "package-c".to_string(),
              "package-c@3.0.0_package-peer@4.0.0".to_string(),
            ),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0_package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@4.0.0".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@3.0.0_package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@4.0.0".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-a@1".to_string(),
          "package-a@1.0.0_package-peer@4.0.0".to_string()
        ),
        (
          "package-peer@4.0.0".to_string(),
          "package-peer@4.0.0".to_string()
        )
      ]
    );
  }

  #[tokio::test]
  async fn resolve_with_peer_deps_ancestor_sibling_not_top_tree() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-0", "1.1.1");
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.ensure_package_version("package-c", "3.0.0");
    api.ensure_package_version("package-peer", "4.0.0");
    api.ensure_package_version("package-peer", "4.1.0");
    api.add_dependency(("package-0", "1.1.1"), ("package-a", "1"));
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "^2"));
    api.add_dependency(("package-a", "1.0.0"), ("package-c", "^3"));
    // the peer dependency is specified here as a sibling of "a" and "b"
    // so it should resolve to 4.0.0 instead of 4.1.0
    api.add_dependency(("package-a", "1.0.0"), ("package-peer", "4.0.0"));
    api.add_peer_dependency(("package-b", "2.0.0"), ("package-peer", "4"));
    api.add_peer_dependency(("package-c", "3.0.0"), ("package-peer", "*"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-0@1.1.1"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-0@1.1.1".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0_package-peer@4.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-b".to_string(),
              "package-b@2.0.0_package-peer@4.0.0".to_string(),
            ),
            (
              "package-c".to_string(),
              "package-c@3.0.0_package-peer@4.0.0".to_string(),
            ),
            ("package-peer".to_string(), "package-peer@4.0.0".to_string(),),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0_package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@4.0.0".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@3.0.0_package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@4.0.0".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-0@1.1.1".to_string(), "package-0@1.1.1".to_string())]
    );
  }

  #[tokio::test]
  async fn resolve_with_peer_deps_auto_resolved() {
    // in this case, the peer dependency is not found in the tree
    // so it's auto-resolved based on the registry
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.ensure_package_version("package-c", "3.0.0");
    api.ensure_package_version("package-peer", "4.0.0");
    api.ensure_package_version("package-peer", "4.1.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "^2"));
    api.add_dependency(("package-a", "1.0.0"), ("package-c", "^3"));
    api.add_peer_dependency(("package-b", "2.0.0"), ("package-peer", "4"));
    api.add_peer_dependency(("package-c", "3.0.0"), ("package-peer", "*"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-b".to_string(),
              "package-b@2.0.0_package-peer@4.1.0".to_string(),
            ),
            (
              "package-c".to_string(),
              "package-c@3.0.0_package-peer@4.1.0".to_string(),
            ),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0_package-peer@4.1.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@4.1.0".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@3.0.0_package-peer@4.1.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@4.1.0".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@4.1.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn resolve_with_optional_peer_dep_not_resolved() {
    // in this case, the peer dependency is not found in the tree
    // so it's auto-resolved based on the registry
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.ensure_package_version("package-c", "3.0.0");
    api.ensure_package_version("package-peer", "4.0.0");
    api.ensure_package_version("package-peer", "4.1.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "^2"));
    api.add_dependency(("package-a", "1.0.0"), ("package-c", "^3"));
    api.add_optional_peer_dependency(
      ("package-b", "2.0.0"),
      ("package-peer", "4"),
    );
    api.add_optional_peer_dependency(
      ("package-c", "3.0.0"),
      ("package-peer", "*"),
    );

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            ("package-b".to_string(), "package-b@2.0.0".to_string(),),
            ("package-c".to_string(), "package-c@3.0.0".to_string(),),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@3.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn resolve_with_optional_peer_found() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.ensure_package_version("package-c", "3.0.0");
    api.ensure_package_version("package-peer", "4.0.0");
    api.ensure_package_version("package-peer", "4.1.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "^2"));
    api.add_dependency(("package-a", "1.0.0"), ("package-c", "^3"));
    api.add_optional_peer_dependency(
      ("package-b", "2.0.0"),
      ("package-peer", "4"),
    );
    api.add_optional_peer_dependency(
      ("package-c", "3.0.0"),
      ("package-peer", "*"),
    );

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec!["npm:package-a@1", "npm:package-peer@4.0.0"],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-b".to_string(),
              "package-b@2.0.0_package-peer@4.0.0".to_string(),
            ),
            (
              "package-c".to_string(),
              "package-c@3.0.0_package-peer@4.0.0".to_string(),
            ),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0_package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@4.0.0".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@3.0.0_package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@4.0.0".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@4.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-a@1".to_string(),
          "package-a@1.0.0_package-peer@4.0.0".to_string()
        ),
        (
          "package-peer@4.0.0".to_string(),
          "package-peer@4.0.0".to_string()
        )
      ]
    );
  }

  #[tokio::test]
  async fn resolve_optional_peer_first_not_resolved_second_resolved_scenario1()
  {
    // When resolving a dependency a second time and it has an optional
    // peer dependency that wasn't previously resolved, it should resolve all the
    // previous versions to the new one
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-peer", "1.0.0");
    api.ensure_package_version("package-peer-unresolved", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "^1"));
    api.add_dependency(("package-a", "1.0.0"), ("package-peer", "^1"));
    api.add_optional_peer_dependency(
      ("package-b", "1.0.0"),
      ("package-peer", "*"),
    );
    api.add_optional_peer_dependency(
      ("package-b", "1.0.0"),
      ("package-peer-unresolved", "*"),
    );

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec!["npm:package-a@1", "npm:package-b@1"],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-b".to_string(),
              "package-b@1.0.0_package-peer@1.0.0".to_string(),
            ),
            ("package-peer".to_string(), "package-peer@1.0.0".to_string(),),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-a@1".to_string(),
          "package-a@1.0.0_package-peer@1.0.0".to_string()
        ),
        (
          "package-b@1".to_string(),
          "package-b@1.0.0_package-peer@1.0.0".to_string()
        )
      ]
    );
  }

  #[tokio::test]
  async fn resolve_optional_peer_first_not_resolved_second_resolved_scenario2()
  {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-peer", "2.0.0");
    api.add_optional_peer_dependency(
      ("package-a", "1.0.0"),
      ("package-peer", "*"),
    );
    api.add_dependency(("package-b", "1.0.0"), ("package-a", "1.0.0"));
    api.add_dependency(("package-b", "1.0.0"), ("package-peer", "2.0.0"));

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec!["npm:package-a@1", "npm:package-b@1"],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@2.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@2.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-peer@2.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-a".to_string(),
              "package-a@1.0.0_package-peer@2.0.0".to_string(),
            ),
            ("package-peer".to_string(), "package-peer@2.0.0".to_string(),)
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@2.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-a@1".to_string(),
          "package-a@1.0.0_package-peer@2.0.0".to_string()
        ),
        (
          "package-b@1".to_string(),
          "package-b@1.0.0_package-peer@2.0.0".to_string()
        )
      ]
    );
  }

  #[tokio::test]
  async fn resolve_optional_dep_npm_req_top() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-peer", "1.0.0");
    api.add_optional_peer_dependency(
      ("package-a", "1.0.0"),
      ("package-peer", "*"),
    );

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec!["npm:package-a@1", "npm:package-peer@1"],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-a@1".to_string(),
          "package-a@1.0.0_package-peer@1.0.0".to_string()
        ),
        (
          "package-peer@1".to_string(),
          "package-peer@1.0.0".to_string()
        )
      ]
    );
  }

  #[tokio::test]
  async fn resolve_optional_dep_different_resolution_second_time() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-peer", "1.0.0");
    api.ensure_package_version("package-peer", "2.0.0");
    api.add_optional_peer_dependency(
      ("package-a", "1.0.0"),
      ("package-peer", "*"),
    );
    api.add_dependency(("package-b", "1.0.0"), ("package-a", "1.0.0"));
    api.add_dependency(("package-b", "1.0.0"), ("package-peer", "2.0.0"));

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec![
        "npm:package-a@1",
        "npm:package-b@1",
        "npm:package-peer@1.0.0",
      ],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@2.0.0".to_string(),
          copy_index: 1,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@2.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-peer@2.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            ("package-peer".to_string(), "package-peer@2.0.0".to_string(),),
            (
              "package-a".to_string(),
              "package-a@1.0.0_package-peer@2.0.0".to_string(),
            ),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@2.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-a@1".to_string(),
          "package-a@1.0.0_package-peer@1.0.0".to_string()
        ),
        (
          "package-b@1".to_string(),
          "package-b@1.0.0_package-peer@2.0.0".to_string()
        ),
        (
          "package-peer@1.0.0".to_string(),
          "package-peer@1.0.0".to_string()
        )
      ]
    );
  }
  #[tokio::test]
  async fn resolve_peer_dep_other_specifier_slot() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-peer", "2.0.0");
    // bit of an edge case... probably nobody has ever done this
    api.add_dependency(
      ("package-a", "1.0.0"),
      ("package-peer2", "npm:package-peer@2"),
    );
    api.add_peer_dependency(("package-a", "1.0.0"), ("package-peer", "2"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@2.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            ("package-peer".to_string(), "package-peer@2.0.0".to_string(),),
            (
              "package-peer2".to_string(),
              "package-peer@2.0.0".to_string(),
            ),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@2.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![(
        "package-a@1".to_string(),
        "package-a@1.0.0_package-peer@2.0.0".to_string()
      ),]
    );
  }

  #[tokio::test]
  async fn resolve_nested_peer_deps_auto_resolved() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-0", "1.0.0");
    api.ensure_package_version("package-peer-a", "2.0.0");
    api.ensure_package_version("package-peer-b", "3.0.0");
    api.add_peer_dependency(("package-0", "1.0.0"), ("package-peer-a", "2"));
    api.add_peer_dependency(
      ("package-peer-a", "2.0.0"),
      ("package-peer-b", "3"),
    );

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-0@1.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-0@1.0.0_package-peer-a@2.0.0__package-peer-b@3.0.0"
            .to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer-a".to_string(),
            "package-peer-a@2.0.0_package-peer-b@3.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer-a@2.0.0_package-peer-b@3.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer-b".to_string(),
            "package-peer-b@3.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer-b@3.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![(
        "package-0@1.0".to_string(),
        "package-0@1.0.0_package-peer-a@2.0.0__package-peer-b@3.0.0"
          .to_string()
      )]
    );
  }

  #[tokio::test]
  async fn resolve_nested_peer_deps_ancestor_sibling_deps() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-0", "1.0.0");
    api.ensure_package_version("package-peer-a", "2.0.0");
    api.ensure_package_version("package-peer-b", "3.0.0");
    api.add_dependency(("package-0", "1.0.0"), ("package-peer-b", "*"));
    api.add_peer_dependency(("package-0", "1.0.0"), ("package-peer-a", "2"));
    api.add_peer_dependency(
      ("package-peer-a", "2.0.0"),
      ("package-peer-b", "3"),
    );

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec![
        "npm:package-0@1.0",
        "npm:package-peer-a@2",
        "npm:package-peer-b@3",
      ],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-0@1.0.0_package-peer-a@2.0.0__package-peer-b@3.0.0_package-peer-b@3.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-peer-a".to_string(),
              "package-peer-a@2.0.0_package-peer-b@3.0.0".to_string(),
            ),
            (
              "package-peer-b".to_string(),
              "package-peer-b@3.0.0".to_string(),
            )
          ]),

        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer-a@2.0.0_package-peer-b@3.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer-b".to_string(),
            "package-peer-b@3.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer-b@3.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),

        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-0@1.0".to_string(),
          "package-0@1.0.0_package-peer-a@2.0.0__package-peer-b@3.0.0_package-peer-b@3.0.0"
            .to_string()
        ),
        (
          "package-peer-a@2".to_string(),
          "package-peer-a@2.0.0_package-peer-b@3.0.0".to_string()
        ),
        (
          "package-peer-b@3".to_string(),
          "package-peer-b@3.0.0".to_string()
        )
      ]
    );
  }

  #[tokio::test]
  async fn resolve_with_peer_deps_multiple() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-0", "1.1.1");
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.ensure_package_version("package-c", "3.0.0");
    api.ensure_package_version("package-d", "3.5.0");
    api.ensure_package_version("package-e", "3.6.0");
    api.ensure_package_version("package-peer-a", "4.0.0");
    api.ensure_package_version("package-peer-a", "4.1.0");
    api.ensure_package_version("package-peer-b", "5.3.0");
    api.ensure_package_version("package-peer-b", "5.4.1");
    api.ensure_package_version("package-peer-c", "6.2.0");
    api.add_dependency(("package-0", "1.1.1"), ("package-a", "1"));
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "^2"));
    api.add_dependency(("package-a", "1.0.0"), ("package-c", "^3"));
    api.add_dependency(("package-a", "1.0.0"), ("package-d", "^3"));
    api.add_dependency(("package-a", "1.0.0"), ("package-peer-a", "4.0.0"));
    api.add_peer_dependency(("package-b", "2.0.0"), ("package-peer-a", "4"));
    api.add_peer_dependency(
      ("package-b", "2.0.0"),
      ("package-peer-c", "=6.2.0"), // will be auto-resolved
    );
    api.add_peer_dependency(("package-c", "3.0.0"), ("package-peer-a", "*"));
    api.add_peer_dependency(
      ("package-peer-a", "4.0.0"),
      ("package-peer-b", "^5.4"), // will be auto-resolved
    );

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec!["npm:package-0@1.1.1", "npm:package-e@3"],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-0@1.1.1".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0_package-peer-a@4.0.0__package-peer-b@5.4.1".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer-a@4.0.0__package-peer-b@5.4.1".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-b".to_string(),
              "package-b@2.0.0_package-peer-a@4.0.0__package-peer-b@5.4.1_package-peer-c@6.2.0".to_string(),
            ),
            (
              "package-c".to_string(),
              "package-c@3.0.0_package-peer-a@4.0.0__package-peer-b@5.4.1".to_string(),
            ),
            (
              "package-d".to_string(),
              "package-d@3.5.0".to_string(),
            ),
            (
              "package-peer-a".to_string(),
              "package-peer-a@4.0.0_package-peer-b@5.4.1".to_string(),
            ),
          ]),

        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0_package-peer-a@4.0.0__package-peer-b@5.4.1_package-peer-c@6.2.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-peer-a".to_string(),
              "package-peer-a@4.0.0_package-peer-b@5.4.1".to_string(),
            ),
            (
              "package-peer-c".to_string(),
              "package-peer-c@6.2.0".to_string(),
            )
          ])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@3.0.0_package-peer-a@4.0.0__package-peer-b@5.4.1".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer-a".to_string(),
            "package-peer-a@4.0.0_package-peer-b@5.4.1".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-d@3.5.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([]),

        },
        TestNpmResolutionPackage {
          pkg_id: "package-e@3.6.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([]),

        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer-a@4.0.0_package-peer-b@5.4.1".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer-b".to_string(),
            "package-peer-b@5.4.1".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer-b@5.4.1".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer-c@6.2.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        ("package-0@1.1.1".to_string(), "package-0@1.1.1".to_string()),
        ("package-e@3".to_string(), "package-e@3.6.0".to_string()),
      ]
    );
  }

  #[tokio::test]
  async fn resolve_peer_deps_circular() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "*"));
    api.add_peer_dependency(("package-b", "2.0.0"), ("package-a", "1"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@2.0.0_package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0".to_string(),
          )]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1.0".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn resolve_peer_deps_multiple_copies() {
    // repeat this a few times to have a higher probability of surfacing indeterminism
    for _ in 0..3 {
      let api = TestNpmRegistryApi::default();
      api.ensure_package_version("package-a", "1.0.0");
      api.ensure_package_version("package-b", "2.0.0");
      api.ensure_package_version("package-dep", "3.0.0");
      api.ensure_package_version("package-peer", "4.0.0");
      api.ensure_package_version("package-peer", "5.0.0");
      api.add_dependency(("package-a", "1.0.0"), ("package-dep", "*"));
      api.add_dependency(("package-a", "1.0.0"), ("package-peer", "4"));
      api.add_dependency(("package-b", "2.0.0"), ("package-dep", "*"));
      api.add_dependency(("package-b", "2.0.0"), ("package-peer", "5"));
      api.add_peer_dependency(("package-dep", "3.0.0"), ("package-peer", "*"));

      let (packages, package_reqs) = run_resolver_and_get_output(
        api,
        vec!["npm:package-a@1", "npm:package-b@2"],
      )
      .await;
      assert_eq!(
        packages,
        vec![
          TestNpmResolutionPackage {
            pkg_id: "package-a@1.0.0_package-peer@4.0.0".to_string(),
            copy_index: 0,
            dependencies: BTreeMap::from([
              (
                "package-dep".to_string(),
                "package-dep@3.0.0_package-peer@4.0.0".to_string(),
              ),
              ("package-peer".to_string(), "package-peer@4.0.0".to_string(),),
            ]),
          },
          TestNpmResolutionPackage {
            pkg_id: "package-b@2.0.0_package-peer@5.0.0".to_string(),
            copy_index: 0,
            dependencies: BTreeMap::from([
              (
                "package-dep".to_string(),
                "package-dep@3.0.0_package-peer@5.0.0".to_string(),
              ),
              ("package-peer".to_string(), "package-peer@5.0.0".to_string(),),
            ]),
          },
          TestNpmResolutionPackage {
            pkg_id: "package-dep@3.0.0_package-peer@4.0.0".to_string(),
            copy_index: 0,
            dependencies: BTreeMap::from([(
              "package-peer".to_string(),
              "package-peer@4.0.0".to_string(),
            )]),
          },
          TestNpmResolutionPackage {
            pkg_id: "package-dep@3.0.0_package-peer@5.0.0".to_string(),
            copy_index: 1,
            dependencies: BTreeMap::from([(
              "package-peer".to_string(),
              "package-peer@5.0.0".to_string(),
            )]),
          },
          TestNpmResolutionPackage {
            pkg_id: "package-peer@4.0.0".to_string(),
            copy_index: 0,
            dependencies: Default::default(),
          },
          TestNpmResolutionPackage {
            pkg_id: "package-peer@5.0.0".to_string(),
            copy_index: 0,
            dependencies: Default::default(),
          },
        ]
      );
      assert_eq!(
        package_reqs,
        vec![
          (
            "package-a@1".to_string(),
            "package-a@1.0.0_package-peer@4.0.0".to_string()
          ),
          (
            "package-b@2".to_string(),
            "package-b@2.0.0_package-peer@5.0.0".to_string()
          )
        ]
      );
    }
  }

  #[tokio::test]
  async fn resolve_dep_with_peer_deps_dep_then_peer() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.ensure_package_version("package-peer", "1.0.0");
    api.add_peer_dependency(("package-b", "1.0.0"), ("package-peer", "1"));
    api.add_dependency(("package-a", "1.0.0"), ("package-c", "1"));
    api.add_dependency(("package-a", "1.0.0"), ("package-peer", "1"));
    api.add_peer_dependency(("package-c", "1.0.0"), ("package-b", "1"));

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec!["npm:package-a@1.0", "npm:package-b@1.0"],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-b@1.0.0__package-peer@1.0.0"
            .to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-c".to_string(),
              "package-c@1.0.0_package-b@1.0.0__package-peer@1.0.0".to_string(),
            ),
            ("package-peer".to_string(), "package-peer@1.0.0".to_string(),)
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@1.0.0_package-b@1.0.0__package-peer@1.0.0"
            .to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0_package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-a@1.0".to_string(),
          "package-a@1.0.0_package-b@1.0.0__package-peer@1.0.0".to_string()
        ),
        (
          "package-b@1.0".to_string(),
          "package-b@1.0.0_package-peer@1.0.0".to_string()
        )
      ]
    );
  }

  #[tokio::test]
  async fn resolve_dep_with_peer_deps_then_other_dep_with_different_peer() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.ensure_package_version("package-peer", "1.1.0");
    api.ensure_package_version("package-peer", "1.2.0");
    api.add_peer_dependency(("package-a", "1.0.0"), ("package-peer", "*")); // should select 1.2.0, then 1.1.0
    api.add_dependency(("package-b", "1.0.0"), ("package-c", "1"));
    api.add_dependency(("package-b", "1.0.0"), ("package-peer", "=1.1.0"));
    api.add_dependency(("package-c", "1.0.0"), ("package-a", "1"));

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec!["npm:package-a@1.0", "npm:package-b@1.0"],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@1.1.0".to_string(),
          copy_index: 1,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@1.1.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@1.2.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@1.2.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-peer@1.1.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-c".to_string(),
              "package-c@1.0.0_package-peer@1.1.0".to_string(),
            ),
            ("package-peer".to_string(), "package-peer@1.1.0".to_string(),)
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@1.0.0_package-peer@1.1.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0_package-peer@1.1.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@1.1.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@1.2.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-a@1.0".to_string(),
          "package-a@1.0.0_package-peer@1.2.0".to_string()
        ),
        (
          "package-b@1.0".to_string(),
          "package-b@1.0.0_package-peer@1.1.0".to_string()
        )
      ]
    );
  }

  #[tokio::test]
  async fn resolve_dep_and_peer_dist_tag() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.ensure_package_version("package-b", "3.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.ensure_package_version("package-d", "1.0.0");
    api.ensure_package_version("package-e", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "some-tag"));
    api.add_dependency(("package-a", "1.0.0"), ("package-d", "1.0.0"));
    api.add_dependency(("package-a", "1.0.0"), ("package-c", "1.0.0"));
    api.add_dependency(("package-a", "1.0.0"), ("package-e", "1.0.0"));
    api.add_dependency(("package-e", "1.0.0"), ("package-b", "some-tag"));
    api.add_peer_dependency(("package-c", "1.0.0"), ("package-d", "other-tag"));
    api.add_dist_tag("package-b", "some-tag", "2.0.0");
    api.add_dist_tag("package-d", "other-tag", "1.0.0");

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-d@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            ("package-b".to_string(), "package-b@2.0.0".to_string(),),
            (
              "package-c".to_string(),
              "package-c@1.0.0_package-d@1.0.0".to_string(),
            ),
            ("package-d".to_string(), "package-d@1.0.0".to_string(),),
            ("package-e".to_string(), "package-e@1.0.0".to_string(),),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@1.0.0_package-d@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-d".to_string(),
            "package-d@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-d@1.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-e@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@2.0.0".to_string(),
          )]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![(
        "package-a@1.0".to_string(),
        "package-a@1.0.0_package-d@1.0.0".to_string()
      ),]
    );
  }

  #[tokio::test]
  async fn package_has_self_as_dependency() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-a", "1"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0"]).await;
    assert_eq!(
      packages,
      vec![TestNpmResolutionPackage {
        pkg_id: "package-a@1.0.0".to_string(),
        copy_index: 0,
        // in this case, we just ignore that the package did this
        dependencies: Default::default(),
      }]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1.0".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn package_has_self_but_different_version_as_dependency() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-a", "0.5.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-a", "^0.5"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@0.5.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@0.5.0".to_string(),
          )]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1.0".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn grand_child_package_has_self_as_peer_dependency_root() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "2"));
    api.add_peer_dependency(("package-b", "2.0.0"), ("package-a", "*"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@2.0.0_package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0".to_string(),
          )]),
        }
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1.0".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn grand_child_package_has_self_as_peer_dependency_under_root() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-0", "1.0.0");
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "2.0.0");
    api.add_dependency(("package-0", "1.0.0"), ("package-a", "*"));
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "2"));
    api.add_peer_dependency(("package-b", "2.0.0"), ("package-a", "*"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-0@1.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-0@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@2.0.0_package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@2.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0".to_string(),
          )]),
        }
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-0@1.0".to_string(), "package-0@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn resolve_peer_deps_in_ancestor_root() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_dependency(("package-b", "1.0.0"), ("package-c", "1"));
    api.add_peer_dependency(("package-c", "1.0.0"), ("package-a", "1"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0_package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-c".to_string(),
            "package-c@1.0.0_package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@1.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0".to_string(),
          )]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1.0.0".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn resolve_peer_deps_in_ancestor_non_root() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_dependency(("package-b", "1.0.0"), ("package-c", "1"));
    api.add_peer_dependency(("package-c", "1.0.0"), ("package-b", "1"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-c".to_string(),
            "package-c@1.0.0_package-b@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@1.0.0_package-b@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0".to_string(),
          )]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1.0.0".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn nested_deps_same_peer_dep_ancestor() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-0", "1.0.0");
    api.ensure_package_version("package-1", "1.0.0");
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.ensure_package_version("package-d", "1.0.0");
    api.add_dependency(("package-0", "1.0.0"), ("package-a", "1"));
    api.add_dependency(("package-0", "1.0.0"), ("package-1", "1"));
    api.add_dependency(("package-1", "1.0.0"), ("package-a", "1"));
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_dependency(("package-b", "1.0.0"), ("package-c", "1"));
    api.add_dependency(("package-c", "1.0.0"), ("package-d", "1"));
    api.add_peer_dependency(("package-b", "1.0.0"), ("package-a", "*"));
    api.add_peer_dependency(("package-c", "1.0.0"), ("package-a", "*"));
    api.add_peer_dependency(("package-d", "1.0.0"), ("package-a", "*"));
    api.add_peer_dependency(("package-b", "1.0.0"), ("package-0", "*"));
    api.add_peer_dependency(("package-c", "1.0.0"), ("package-0", "*"));
    api.add_peer_dependency(("package-d", "1.0.0"), ("package-0", "*"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-0@1.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-0@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0_package-0@1.0.0".to_string(),
          ), (
            "package-1".to_string(),
            "package-1@1.0.0_package-0@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-1@1.0.0_package-0@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0_package-0@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-0@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0_package-0@1.0.0_package-a@1.0.0__package-0@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-0@1.0.0_package-a@1.0.0__package-0@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-0".to_string(),
              "package-0@1.0.0".to_string(),
            ),
            (
              "package-a".to_string(),
              "package-a@1.0.0_package-0@1.0.0".to_string(),
            ),
            (
              "package-c".to_string(),
              "package-c@1.0.0_package-0@1.0.0_package-a@1.0.0__package-0@1.0.0".to_string(),
            )
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@1.0.0_package-0@1.0.0_package-a@1.0.0__package-0@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-0".to_string(),
              "package-0@1.0.0".to_string(),
            ),
            (
              "package-a".to_string(),
              "package-a@1.0.0_package-0@1.0.0".to_string(),
            ),
            (
              "package-d".to_string(),
              "package-d@1.0.0_package-0@1.0.0_package-a@1.0.0__package-0@1.0.0".to_string(),
            )
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-d@1.0.0_package-0@1.0.0_package-a@1.0.0__package-0@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-0".to_string(),
              "package-0@1.0.0".to_string(),
            ),
            (
              "package-a".to_string(),
              "package-a@1.0.0_package-0@1.0.0".to_string(),
            )
          ]),
        }
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-0@1.0".to_string(), "package-0@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn peer_dep_resolved_then_resolved_deeper() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-0", "1.0.0");
    api.ensure_package_version("package-1", "1.0.0");
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-peer", "1.0.0");
    api.add_dependency(("package-0", "1.0.0"), ("package-a", "1"));
    api.add_dependency(("package-0", "1.0.0"), ("package-1", "1"));
    api.add_dependency(("package-1", "1.0.0"), ("package-a", "1"));
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_peer_dependency(("package-b", "1.0.0"), ("package-peer", "*"));

    let (packages, package_reqs) = run_resolver_and_get_output(
      api,
      vec!["npm:package-0@1.0", "npm:package-peer@1.0"],
    )
    .await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-0@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-1".to_string(),
              "package-1@1.0.0_package-peer@1.0.0".to_string(),
            ),
            (
              "package-a".to_string(),
              "package-a@1.0.0_package-peer@1.0.0".to_string(),
            )
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-1@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0_package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0_package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-peer".to_string(),
            "package-peer@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-peer@1.0.0".to_string(),
          copy_index: 0,
          dependencies: Default::default(),
        }
      ]
    );
    assert_eq!(
      package_reqs,
      vec![
        (
          "package-0@1.0".to_string(),
          "package-0@1.0.0_package-peer@1.0.0".to_string()
        ),
        (
          "package-peer@1.0".to_string(),
          "package-peer@1.0.0".to_string()
        )
      ]
    );
  }

  #[tokio::test]
  async fn resolve_dep_with_peer_deps_circular_1() {
    // a -> b -> c -> d -> c where c has a peer dependency on b
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.ensure_package_version("package-d", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_dependency(("package-b", "1.0.0"), ("package-c", "1"));
    api.add_dependency(("package-c", "1.0.0"), ("package-d", "1"));
    api.add_dependency(("package-d", "1.0.0"), ("package-c", "1"));
    api.add_peer_dependency(("package-c", "1.0.0"), ("package-b", "1"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-c".to_string(),
            "package-c@1.0.0_package-b@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@1.0.0_package-b@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            ("package-b".to_string(), "package-b@1.0.0".to_string(),),
            (
              "package-d".to_string(),
              "package-d@1.0.0_package-b@1.0.0".to_string(),
            )
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-d@1.0.0_package-b@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-c".to_string(),
            "package-c@1.0.0_package-b@1.0.0".to_string(),
          )]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1.0.0".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn resolve_dep_with_peer_deps_circular_2() {
    // a -> b -> c -> d -> c where c has a peer dependency on b
    //             -> e -> f -> d -> c where f has a peer dep on a
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.ensure_package_version("package-d", "1.0.0");
    api.ensure_package_version("package-e", "1.0.0");
    api.ensure_package_version("package-f", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_dependency(("package-b", "1.0.0"), ("package-c", "1"));
    api.add_dependency(("package-c", "1.0.0"), ("package-d", "1"));
    api.add_dependency(("package-c", "1.0.0"), ("package-e", "1"));
    api.add_dependency(("package-d", "1.0.0"), ("package-c", "1"));
    api.add_dependency(("package-e", "1.0.0"), ("package-f", "1"));
    api.add_dependency(("package-f", "1.0.0"), ("package-d", "1"));
    api.add_peer_dependency(("package-f", "1.0.0"), ("package-a", "1"));
    api.add_peer_dependency(("package-c", "1.0.0"), ("package-b", "1"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0_package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-c".to_string(),
            "package-c@1.0.0_package-b@1.0.0__package-a@1.0.0_package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@1.0.0_package-b@1.0.0__package-a@1.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-b".to_string(),
              "package-b@1.0.0_package-a@1.0.0".to_string(),
            ),
            (
              "package-d".to_string(),
              "package-d@1.0.0_package-b@1.0.0__package-a@1.0.0_package-a@1.0.0".to_string(),
            ),
            (
              "package-e".to_string(),
              "package-e@1.0.0_package-a@1.0.0_package-b@1.0.0__package-a@1.0.0".to_string()
            )
          ]),

        },
        TestNpmResolutionPackage {
          pkg_id: "package-d@1.0.0_package-b@1.0.0__package-a@1.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-c".to_string(),
            "package-c@1.0.0_package-b@1.0.0__package-a@1.0.0_package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-e@1.0.0_package-a@1.0.0_package-b@1.0.0__package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-f".to_string(),
            "package-f@1.0.0_package-a@1.0.0_package-b@1.0.0__package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-f@1.0.0_package-a@1.0.0_package-b@1.0.0__package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0".to_string(),
          ), (
            "package-d".to_string(),
            "package-d@1.0.0_package-b@1.0.0__package-a@1.0.0_package-a@1.0.0".to_string(),
          )]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1.0.0".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn resolve_dep_with_peer_deps_circular_3() {
    // a -> b -> c -> d -> c (peer)
    //                  -> e -> a (peer)
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.ensure_package_version("package-d", "1.0.0");
    api.ensure_package_version("package-e", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_dependency(("package-b", "1.0.0"), ("package-c", "1"));
    api.add_dependency(("package-c", "1.0.0"), ("package-d", "1"));
    api.add_dependency(("package-d", "1.0.0"), ("package-e", "1"));
    api.add_peer_dependency(("package-d", "1.0.0"), ("package-c", "1"));
    api.add_peer_dependency(("package-e", "1.0.0"), ("package-a", "1"));

    let (packages, package_reqs) =
      run_resolver_and_get_output(api, vec!["npm:package-a@1.0.0"]).await;
    assert_eq!(
      packages,
      vec![
        TestNpmResolutionPackage {
          pkg_id: "package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-b".to_string(),
            "package-b@1.0.0_package-a@1.0.0".to_string(),
          )])
        },
        TestNpmResolutionPackage {
          pkg_id: "package-b@1.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-c".to_string(),
            "package-c@1.0.0_package-a@1.0.0".to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-c@1.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-d".to_string(),
            "package-d@1.0.0_package-c@1.0.0__package-a@1.0.0_package-a@1.0.0"
              .to_string(),
          )]),
        },
        TestNpmResolutionPackage {
          pkg_id:
            "package-d@1.0.0_package-c@1.0.0__package-a@1.0.0_package-a@1.0.0"
              .to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([
            (
              "package-c".to_string(),
              "package-c@1.0.0_package-a@1.0.0".to_string(),
            ),
            (
              "package-e".to_string(),
              "package-e@1.0.0_package-a@1.0.0".to_string()
            ),
          ]),
        },
        TestNpmResolutionPackage {
          pkg_id: "package-e@1.0.0_package-a@1.0.0".to_string(),
          copy_index: 0,
          dependencies: BTreeMap::from([(
            "package-a".to_string(),
            "package-a@1.0.0".to_string()
          )]),
        },
      ]
    );
    assert_eq!(
      package_reqs,
      vec![("package-a@1.0.0".to_string(), "package-a@1.0.0".to_string())]
    );
  }

  #[tokio::test]
  async fn resolve_optional_deps() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b", "1.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.ensure_package_version("package-d", "1.0.0");
    api.ensure_package_version("package-e", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b", "1"));
    api.add_optional_dependency(("package-a", "1.0.0"), ("package-c", "1"));
    api.add_dependency(("package-c", "1.0.0"), ("package-d", "1"));
    api.add_optional_dependency(("package-d", "1.0.0"), ("package-e", "1"));
    api.with_version_info(("package-c", "1.0.0"), |info| {
      info.os = vec!["win32".to_string(), "darwin".to_string()];
    });
    api.with_version_info(("package-e", "1.0.0"), |info| {
      info.os = vec!["win32".to_string()];
    });

    let snapshot =
      run_resolver_and_get_snapshot(api, vec!["npm:package-a@1.0.0"]).await;
    let packages = package_names_with_info(
      &snapshot,
      &NpmSystemInfo {
        os: "win32".to_string(),
        cpu: "x86".to_string(),
      },
    );
    assert_eq!(
      packages,
      vec![
        "package-a@1.0.0".to_string(),
        "package-b@1.0.0".to_string(),
        "package-c@1.0.0".to_string(),
        "package-d@1.0.0".to_string(),
        "package-e@1.0.0".to_string(),
      ]
    );

    let packages = package_names_with_info(
      &snapshot,
      &NpmSystemInfo {
        os: "darwin".to_string(),
        cpu: "x86".to_string(),
      },
    );
    assert_eq!(
      packages,
      vec![
        "package-a@1.0.0".to_string(),
        "package-b@1.0.0".to_string(),
        "package-c@1.0.0".to_string(),
        "package-d@1.0.0".to_string(),
      ]
    );

    let packages = package_names_with_info(
      &snapshot,
      &NpmSystemInfo {
        os: "linux".to_string(),
        cpu: "x86".to_string(),
      },
    );
    assert_eq!(
      packages,
      vec!["package-a@1.0.0".to_string(), "package-b@1.0.0".to_string()]
    );
  }

  #[tokio::test]
  async fn resolve_optional_to_required() {
    let api = TestNpmRegistryApi::default();
    api.ensure_package_version("package-a", "1.0.0");
    api.ensure_package_version("package-b1", "1.0.0");
    api.ensure_package_version("package-b2", "1.0.0");
    api.ensure_package_version("package-b3", "1.0.0");
    api.ensure_package_version("package-c", "1.0.0");
    api.ensure_package_version("package-d", "1.0.0");
    api.ensure_package_version("package-e", "1.0.0");
    api.add_dependency(("package-a", "1.0.0"), ("package-b1", "1"));
    api.add_dependency(("package-b1", "1.0.0"), ("package-b2", "1"));
    api.add_dependency(("package-b2", "1.0.0"), ("package-b3", "1"));
    // deep down this is set back to being required, so it and its required
    // dependency should be marked as required
    api.add_dependency(("package-b3", "1.0.0"), ("package-c", "1"));
    api.add_optional_dependency(("package-a", "1.0.0"), ("package-c", "1"));
    api.add_dependency(("package-c", "1.0.0"), ("package-d", "1"));
    api.add_optional_dependency(("package-d", "1.0.0"), ("package-e", "1"));

    api.with_version_info(("package-c", "1.0.0"), |info| {
      info.os = vec!["win32".to_string()];
    });
    api.with_version_info(("package-e", "1.0.0"), |info| {
      info.os = vec!["win32".to_string()];
    });

    let snapshot =
      run_resolver_and_get_snapshot(api, vec!["npm:package-a@1.0.0"]).await;

    let packages = package_names_with_info(
      &snapshot,
      &NpmSystemInfo {
        os: "darwin".to_string(),
        cpu: "x86".to_string(),
      },
    );
    assert_eq!(
      packages,
      vec![
        "package-a@1.0.0".to_string(),
        "package-b1@1.0.0".to_string(),
        "package-b2@1.0.0".to_string(),
        "package-b3@1.0.0".to_string(),
        "package-c@1.0.0".to_string(),
        "package-d@1.0.0".to_string(),
      ]
    );
  }

  #[derive(Debug, PartialEq, Eq)]
  struct TestNpmResolutionPackage {
    pub pkg_id: String,
    pub copy_index: u8,
    pub dependencies: BTreeMap<String, String>,
  }

  async fn run_resolver_and_get_output(
    api: TestNpmRegistryApi,
    reqs: Vec<&str>,
  ) -> (Vec<TestNpmResolutionPackage>, Vec<(String, String)>) {
    let snapshot = run_resolver_and_get_snapshot(api, reqs).await;
    let mut packages = snapshot
      .all_packages_for_every_system()
      .cloned()
      .collect::<Vec<_>>();
    packages.sort_by(|a, b| a.id.cmp(&b.id));
    let mut package_reqs = snapshot
      .package_reqs
      .into_iter()
      .map(|(a, b)| {
        (
          a.to_string(),
          snapshot.root_packages.get(&b).unwrap().as_serialized(),
        )
      })
      .collect::<Vec<_>>();
    package_reqs.sort_by(|a, b| a.0.to_string().cmp(&b.0.to_string()));

    let packages = packages
      .into_iter()
      .map(|pkg| TestNpmResolutionPackage {
        pkg_id: pkg.id.as_serialized(),
        copy_index: pkg.copy_index,
        dependencies: pkg
          .dependencies
          .into_iter()
          .map(|(key, value)| (key, value.as_serialized()))
          .collect(),
      })
      .collect();

    (packages, package_reqs)
  }

  fn package_names_with_info(
    snapshot: &NpmResolutionSnapshot,
    system_info: &NpmSystemInfo,
  ) -> Vec<String> {
    let mut packages = snapshot
      .all_system_packages(system_info)
      .into_iter()
      .map(|p| p.id.as_serialized())
      .collect::<Vec<_>>();
    packages.sort();
    let mut serialized_pkgs = snapshot
      .as_valid_serialized_for_system(system_info)
      .into_serialized()
      .packages
      .into_iter()
      .map(|p| p.id.as_serialized())
      .collect::<Vec<_>>();
    serialized_pkgs.sort();
    // ensure the output of both of these are the same
    assert_eq!(serialized_pkgs, packages);
    packages
  }

  async fn run_resolver_and_get_snapshot(
    api: TestNpmRegistryApi,
    reqs: Vec<&str>,
  ) -> NpmResolutionSnapshot {
    fn snapshot_to_serialized(
      snapshot: &NpmResolutionSnapshot,
    ) -> SerializedNpmResolutionSnapshot {
      let mut snapshot = snapshot.as_valid_serialized().into_serialized();
      snapshot.packages.sort_by(|a, b| a.id.cmp(&b.id));
      snapshot
    }

    let snapshot =
      NpmResolutionSnapshot::new(NpmResolutionSnapshotCreateOptions {
        api: Arc::new(api),
        snapshot: Default::default(),
        types_node_version_req: None,
      });
    let (mut graph, api, version_resolver) = Graph::from_snapshot(snapshot);
    let npm_version_resolver = NpmVersionResolver {
      types_node_version_req: None,
    };
    let mut resolver =
      GraphDependencyResolver::new(&mut graph, &*api, &npm_version_resolver);

    for req in reqs {
      let req = NpmPackageReqReference::from_str(req).unwrap().req;
      resolver
        .add_package_req(&req, &api.package_info(&req.name).await.unwrap())
        .unwrap();
    }

    resolver.resolve_pending().await.unwrap();
    let snapshot = graph.into_snapshot(api, version_resolver).await.unwrap();

    {
      let (graph, api, version_resolver) =
        Graph::from_snapshot(snapshot.clone());
      let new_snapshot =
        graph.into_snapshot(api, version_resolver).await.unwrap();
      assert_eq!(
        snapshot_to_serialized(&snapshot),
        snapshot_to_serialized(&new_snapshot),
        "recreated snapshot should be the same"
      );
      // create one again from the new snapshot
      let (graph, api, version_resolver) =
        Graph::from_snapshot(new_snapshot.clone());
      let new_snapshot2 =
        graph.into_snapshot(api, version_resolver).await.unwrap();
      assert_eq!(
        snapshot_to_serialized(&snapshot),
        snapshot_to_serialized(&new_snapshot2),
        "second recreated snapshot should be the same"
      );
    }

    snapshot
  }
}
