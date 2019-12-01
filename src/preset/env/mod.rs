use crate::config::FileMatcher;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Config {
    #[serde(default)]
    pub debug: bool,

    #[serde(default)]
    pub include: FileMatcher,

    #[serde(default)]
    pub exclude: FileMatcher,
}
