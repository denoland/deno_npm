use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use deno_npm::registry::NpmPackageInfo;
use deno_npm::registry::NpmRegistryApi;
use deno_npm::registry::NpmRegistryPackageInfoLoadError;
use reqwest::StatusCode;

#[tokio::main(flavor = "current_thread")]
async fn main() {}

struct TargetFolderCachedRegistryApi {
  data: Rc<RefCell<HashMap<String, Arc<NpmPackageInfo>>>>,
}

impl Default for TargetFolderCachedRegistryApi {
  fn default() -> Self {
    std::fs::create_dir_all("target/.deno_npm").unwrap();
    Self {
      data: Default::default(),
    }
  }
}

#[async_trait::async_trait(?Send)]
impl NpmRegistryApi for TargetFolderCachedRegistryApi {
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
