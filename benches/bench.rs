use deno_npm::registry::TestNpmRegistryApi;
use deno_npm::resolution::AddPkgReqsOptions;
use deno_npm::resolution::NpmResolutionSnapshot;
use deno_semver::package::PackageReq;

fn main() {
  divan::main();
}

mod resolution {

  use super::*;

  #[divan::bench]
  fn test() {
    let api = TestNpmRegistryApi::default();
    let mut initial_pkgs = Vec::new();
    const VERSION_COUNT: usize = 25;
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
            (next_pkg.as_str(), &version.as_str()),
          );
        }
      }
    }

    let rt = tokio::runtime::Builder::new_current_thread()
      .build()
      .unwrap();
    let snapshot = rt.block_on(async {
      run_resolver_and_get_snapshot(api, initial_pkgs).await
    });

    assert_eq!(snapshot.top_level_packages().count(), VERSION_COUNT);
  }
}

async fn run_resolver_and_get_snapshot(
  api: TestNpmRegistryApi,
  reqs: Vec<String>,
) -> NpmResolutionSnapshot {
  let snapshot = NpmResolutionSnapshot::new(Default::default());
  let reqs = reqs
    .iter()
    .map(|req| PackageReq::from_str(req).unwrap())
    .collect::<Vec<_>>();
  let result = snapshot
    .add_pkg_reqs(
      &api,
      AddPkgReqsOptions {
        package_reqs: &reqs,
        types_node_version_req: None,
      },
    )
    .await;
  result.dep_graph_result.unwrap()
}
