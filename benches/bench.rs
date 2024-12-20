use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

use deno_npm::registry::NpmPackageInfo;
use deno_npm::registry::NpmRegistryApi;
use deno_npm::registry::NpmRegistryPackageInfoLoadError;
use deno_npm::registry::TestNpmRegistryApi;
use deno_npm::resolution::AddPkgReqsOptions;
use deno_npm::resolution::NpmResolutionSnapshot;
use deno_semver::package::PackageReq;
use reqwest::StatusCode;

fn main() {
  divan::main();
}

mod resolution {
  use super::*;

  #[divan::bench]
  fn test(bencher: divan::Bencher) {
    let api = TestNpmRegistryApi::default();
    let mut initial_pkgs = Vec::new();
    const VERSION_COUNT: usize = 10;
    for pkg_index in 0..26 {
      let pkg_name = format!("a{}", pkg_index);
      let next_pkg = format!("a{}", pkg_index + 1);
      for version_index in 0..VERSION_COUNT {
        let version = format!("{}.0.0", version_index);
        if pkg_index == 0 {
          initial_pkgs.push(format!(
            "{}@{}",
            pkg_name.clone(),
            version.clone()
          ));
        }
        api.ensure_package_version(&pkg_name, &version);
        if pkg_index < 25 {
          api.add_dependency(
            (pkg_name.as_str(), version.as_str()),
            (next_pkg.as_str(), version.as_str()),
          );
        }
      }
    }

    let rt = tokio::runtime::Builder::new_current_thread()
      .build()
      .unwrap();

    bencher.bench_local(|| {
      let snapshot = rt.block_on(async {
        run_resolver_and_get_snapshot(&api, &initial_pkgs).await
      });

      assert_eq!(snapshot.top_level_packages().count(), VERSION_COUNT);
    });
  }

  #[divan::bench]
  fn nextjs_resolve(bencher: divan::Bencher) {
    let api = RealBenchRegistryApi::default();
    let rt = tokio::runtime::Builder::new_current_thread()
      .enable_io()
      .enable_time()
      .build()
      .unwrap();

    // run once to fill the caches
    rt.block_on(async {
      run_resolver_and_get_snapshot(&api, &["next@15.1.2".to_string()]).await
    });

    bencher.bench_local(|| {
      let snapshot = rt.block_on(async {
        run_resolver_and_get_snapshot(&api, &["next@15.1.2".to_string()]).await
      });

      assert_eq!(snapshot.top_level_packages().count(), 1);
    });
  }
}

struct RealBenchRegistryApi {
  data: Rc<RefCell<HashMap<String, Arc<NpmPackageInfo>>>>,
}

impl Default for RealBenchRegistryApi {
  fn default() -> Self {
    std::fs::create_dir_all("target/.deno_npm").unwrap();
    Self {
      data: Default::default(),
    }
  }
}

#[async_trait::async_trait(?Send)]
impl NpmRegistryApi for RealBenchRegistryApi {
  async fn package_info(
    &self,
    name: &str,
  ) -> Result<Arc<NpmPackageInfo>, NpmRegistryPackageInfoLoadError> {
    if let Some(data) = self.data.borrow_mut().get(name).cloned() {
      return Ok(data);
    }
    let encoded_name = name.replace("/", "%2F");
    let file_path = format!("target/.deno_npm/{}", encoded_name);
    if let Ok(data) = std::fs::read_to_string(&file_path) {
      if let Ok(data) = serde_json::from_str(&data) {
        return Ok(Arc::new(data));
      }
    }
    let url = format!("https://registry.npmjs.org/{}", encoded_name);
    eprintln!("Downloading {}", url);
    let resp = reqwest::get(&url).await.unwrap();
    if resp.status() == StatusCode::NOT_FOUND {
      return Err(NpmRegistryPackageInfoLoadError::PackageNotExists {
        package_name: name.to_string(),
      });
    }
    let data = resp.json::<NpmPackageInfo>().await.unwrap();
    std::fs::write(&file_path, serde_json::to_string(&data).unwrap()).unwrap();
    let data = Arc::new(data);
    self
      .data
      .borrow_mut()
      .insert(name.to_string(), data.clone());
    Ok(data)
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
      },
    )
    .await;
  result.dep_graph_result.unwrap()
}
