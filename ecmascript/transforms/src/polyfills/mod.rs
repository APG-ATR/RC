mod corejs2_data;

use serde::Deserialize;

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(untagged)]
pub enum Mode {
    #[serde(rename = "usage")]
    Usage,
    #[serde(rename = "entry")]
    Entry,
}

#[derive(Debug, Clone, Copy, Deserialize)]
pub struct Config {
    pub skip: Vec<JsWord>,
}
