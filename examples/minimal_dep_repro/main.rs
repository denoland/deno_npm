use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::sync::Arc;

use deno_npm::registry::NpmPackageInfo;
use deno_npm::registry::NpmPackageVersionInfo;
use deno_npm::registry::NpmRegistryApi;
use deno_npm::registry::NpmRegistryPackageInfoLoadError;
use deno_npm::resolution::AddPkgReqsOptions;
use deno_npm::resolution::NpmResolutionSnapshot;
use deno_semver::package::PackageNv;
use deno_semver::package::PackageReq;
use reqwest::StatusCode;

/// Example:
///
/// ```sh
/// cargo run --example minimal_dep_repro
/// ```
#[tokio::main(flavor = "current_thread")]
async fn main() {
  let (snapshot, api) = minimal_reproduction_while_condition(
    &[
      "@deno/vite-plugin@~1.0.4",
      "@tailwindcss/vite@~4.0.17",
      "tailwindcss@~4.0.17",
      "@types/react@~19.0.10",
      "@types/react-dom@~19.0.4",
      "@vitejs/plugin-react-swc@~3.8.0",
      "react@~19.0.0",
      "react-dom@~19.0.0",
      "vite@~6.1.1",
    ],
    |snapshot| {
      // condition that has the bug
      snapshot
        .all_packages_for_every_system()
        .filter(|p| p.id.nv.name == "vite")
        .count()
        > 1
    },
  )
  .await;

  let test_code = get_test_code(&snapshot, &api);
  eprintln!("===========================");
  eprintln!("Test code");
  eprintln!("===========================");
  println!("{}", test_code);
}

async fn minimal_reproduction_while_condition(
  reqs: &[&str],
  condition: impl Fn(&NpmResolutionSnapshot) -> bool + 'static,
) -> (NpmResolutionSnapshot, SubsetRegistryApi) {
  let reqs = reqs.iter().map(|r| r.to_string()).collect();
  let mut solver = MinimalReproductionSolver::new(reqs, condition).await;
  let mut had_change = true;
  while had_change {
    had_change = false;
    had_change |= solver.attempt_reduce_reqs().await;
    had_change |= solver.attempt_reduce_dependendencies().await;
  }

  (solver.current_snapshot, solver.api)
}

fn get_test_code(
  snapshot: &NpmResolutionSnapshot,
  api: &SubsetRegistryApi,
) -> String {
  let mut text = String::new();

  text.push_str("let api = TestNpmRegistryApi::default();\n");

  let ids = snapshot
    .all_packages_for_every_system()
    .map(|pkg| &pkg.id)
    .collect::<BTreeSet<_>>();
  let nvs = ids.iter().map(|id| &id.nv).collect::<BTreeSet<_>>();

  for nv in &nvs {
    text.push_str(&format!(
      "api.ensure_package_version(\"{}\", \"{}\");\n",
      nv.name, nv.version
    ));
  }

  for nv in &nvs {
    text.push('\n');
    // text.push_str(&format!("// {}\n", nv));
    let version_info = api.get_version_info(nv);
    for (key, value) in &version_info.dependencies {
      text.push_str(&format!(
        "api.add_dependency((\"{}\", \"{}\"), (\"{}\", \"{}\"));\n",
        nv.name, nv.version, key, value
      ));
    }
    for (key, value) in &version_info.peer_dependencies {
      let is_optional = version_info
        .peer_dependencies_meta
        .get(key)
        .map(|m| m.optional)
        .unwrap_or(false);
      if is_optional {
        text.push_str(&format!(
          "api.add_optional_peer_dependency((\"{}\", \"{}\"), (\"{}\", \"{}\"));\n",
          nv.name, nv.version, key, value
        ));
      } else {
        text.push_str(&format!(
          "api.add_peer_dependency((\"{}\", \"{}\"), (\"{}\", \"{}\"));\n",
          nv.name, nv.version, key, value
        ));
      }
    }
  }

  let reqs = snapshot.package_reqs().iter().collect::<BTreeMap<_, _>>();
  let reqs = reqs
    .keys()
    .map(|k| format!("\"{}\"", k))
    .collect::<Vec<_>>();
  text
    .push_str("let (packages, package_reqs) = run_resolver_and_get_output(\n");
  text.push_str("  api,\n");
  text.push_str("  vec![");
  text.push_str(&reqs.join(", "));
  text.push_str("],\n");
  text.push_str(").await;\n");
  text.push_str("assert_eq!(packages, vec![]);\n");
  text.push_str("assert_eq!(package_reqs, vec![]);\n");

  text
}

struct MinimalReproductionSolver {
  reqs: Vec<String>,
  condition: Box<dyn Fn(&NpmResolutionSnapshot) -> bool>,
  api: SubsetRegistryApi,
  current_snapshot: NpmResolutionSnapshot,
}

impl MinimalReproductionSolver {
  pub async fn new(
    reqs: Vec<String>,
    condition: impl Fn(&NpmResolutionSnapshot) -> bool + 'static,
  ) -> Self {
    let api = SubsetRegistryApi::default();
    let snapshot = run_resolver_and_get_snapshot(&api, &reqs).await;
    assert!(condition(&snapshot), "bug does not exist in provided setup");
    MinimalReproductionSolver {
      reqs,
      condition: Box::new(condition),
      api,
      current_snapshot: snapshot,
    }
  }

  pub async fn attempt_reduce_reqs(&mut self) -> bool {
    let mut made_reduction = false;
    for i in (0..self.reqs.len()).rev() {
      let mut new_reqs = self.reqs.clone();
      let removed_req = new_reqs.remove(i);
      let snapshot = run_resolver_and_get_snapshot(&self.api, &new_reqs).await;
      if (self.condition)(&snapshot) {
        self.reqs = new_reqs;
        self.current_snapshot = snapshot;
        made_reduction = true;
        eprintln!("Removed req: {}", removed_req);
      }
    }
    made_reduction
  }

  pub async fn attempt_reduce_dependendencies(&mut self) -> bool {
    let mut made_reduction = false;
    let package_nvs = self
      .current_snapshot
      .all_packages_for_every_system()
      .map(|pkg| pkg.id.nv.clone())
      .collect::<BTreeSet<_>>();

    for package_nv in package_nvs {
      let dep_names = {
        let version_info = self.api.get_version_info(&package_nv);
        version_info
          .dependencies
          .keys()
          .chain(version_info.optional_dependencies.keys())
          .chain(version_info.peer_dependencies.keys())
          .cloned()
          .collect::<BTreeSet<_>>()
      };
      for dep_name in dep_names {
        let new_api = self.api.clone();
        let mut new_version_info =
          self.api.get_version_info(&package_nv).clone();
        new_version_info.dependencies.remove(&dep_name);
        new_version_info.optional_dependencies.remove(&dep_name);
        new_version_info.peer_dependencies.remove(&dep_name);
        new_version_info.peer_dependencies_meta.remove(&dep_name);
        new_api.set_package_version_info(&package_nv, new_version_info);
        let snapshot =
          run_resolver_and_get_snapshot(&new_api, &self.reqs).await;
        if (self.condition)(&snapshot) {
          self.api = new_api;
          self.current_snapshot = snapshot;
          made_reduction = true;
          eprintln!("{}: removed {}", package_nv, dep_name);
        }
      }
    }

    made_reduction
  }
}

async fn run_resolver_and_get_snapshot(
  api: &impl NpmRegistryApi,
  reqs: &[String],
) -> NpmResolutionSnapshot {
  let snapshot = NpmResolutionSnapshot::new(Default::default());
  let reqs = reqs
    .iter()
    .map(|req| PackageReq::from_str(req).unwrap())
    .collect::<Vec<_>>();
  let result = snapshot
    .add_pkg_reqs(
      api,
      AddPkgReqsOptions {
        package_reqs: &reqs,
        types_node_version_req: None,
        patch_packages: &Default::default(),
      },
    )
    .await;
  result.dep_graph_result.unwrap()
}

#[derive(Clone)]
struct SubsetRegistryApi {
  data: RefCell<HashMap<String, Arc<NpmPackageInfo>>>,
}

impl Default for SubsetRegistryApi {
  fn default() -> Self {
    std::fs::create_dir_all("target/.deno_npm").unwrap();
    Self {
      data: Default::default(),
    }
  }
}

impl SubsetRegistryApi {
  pub fn get_version_info(&self, nv: &PackageNv) -> NpmPackageVersionInfo {
    self
      .data
      .borrow()
      .get(nv.name.as_str())
      .unwrap()
      .versions
      .get(&nv.version)
      .unwrap()
      .clone()
  }

  pub fn set_package_version_info(
    &self,
    nv: &PackageNv,
    version_info: NpmPackageVersionInfo,
  ) {
    let mut data = self.data.borrow_mut();
    let mut package_info = data.get(nv.name.as_str()).unwrap().as_ref().clone();
    package_info
      .versions
      .insert(nv.version.clone(), version_info);
    data.insert(nv.name.to_string(), Arc::new(package_info));
  }
}

#[async_trait::async_trait(?Send)]
impl NpmRegistryApi for SubsetRegistryApi {
  async fn package_info(
    &self,
    name: &str,
  ) -> Result<Arc<NpmPackageInfo>, NpmRegistryPackageInfoLoadError> {
    if let Some(data) = self.data.borrow_mut().get(name).cloned() {
      return Ok(data);
    }
    let file_path = packument_cache_filepath(name);
    if let Ok(data) = std::fs::read_to_string(&file_path) {
      if let Ok(data) = serde_json::from_str::<Arc<NpmPackageInfo>>(&data) {
        self
          .data
          .borrow_mut()
          .insert(name.to_string(), data.clone());
        return Ok(data);
      }
    }
    let url = packument_url(name);
    eprintln!("Downloading {}", url);
    let resp = reqwest::get(&url).await.unwrap();
    if resp.status() == StatusCode::NOT_FOUND {
      return Err(NpmRegistryPackageInfoLoadError::PackageNotExists {
        package_name: name.to_string(),
      });
    }
    let text = resp.text().await.unwrap();
    std::fs::write(&file_path, &text).unwrap();
    let data = serde_json::from_str::<Arc<NpmPackageInfo>>(&text).unwrap();
    self
      .data
      .borrow_mut()
      .insert(name.to_string(), data.clone());
    Ok(data)
  }
}

fn packument_cache_filepath(name: &str) -> String {
  format!("target/.deno_npm/{}", encode_package_name(name))
}

fn packument_url(name: &str) -> String {
  format!("https://registry.npmjs.org/{}", encode_package_name(name))
}

fn encode_package_name(name: &str) -> String {
  name.replace("/", "%2F")
}
