#![feature(try_blocks)]
#![feature(test)]

extern crate test;

use serde::Deserialize;
use serde_json::Value;
use std::{collections::HashMap, env, fs::File, io, path::PathBuf, process::Command};
use swc_common::FromVariant;
use test::{TestDesc, TestDescAndFn};
use walkdir::WalkDir;

/// options.json file
#[derive(Debug, Deserialize)]
struct BabelOptions {
    presets: Vec<(String, PresetConfig)>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct PresetConfig {
    #[serde(default)]
    pub use_built_ins: UseBuiltIns,

    #[serde(default)]
    pub corejs: CoreJs,

    #[serde(default)]
    pub modules: ModulesConfig,

    #[serde(default)]
    pub targets: HashMap<String, Value>,

    #[serde(default)]
    pub include: Vec<String>,

    #[serde(default)]
    pub exclude: Vec<String>,

    #[serde(default)]
    pub force_all_transforms: bool,

    #[serde(default)]
    pub shippedProposals: bool,

    #[serde(default)]
    pub config_path: String,

    #[serde(default)]
    pub debug: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
#[serde(untagged)]
pub enum CoreJs {
    Ver(usize),
    VerWithMinor(String),
    Val(HashMap<String, Value>),
}

impl Default for CoreJs {
    fn default() -> Self {
        Self::Ver(2)
    }
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum ModulesConfig {
    Bool(bool),
}

impl Default for ModulesConfig {
    fn default() -> Self {
        ModulesConfig::Bool(false)
    }
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum UseBuiltIns {
    Bool(bool),
    Str(String),
}

impl Default for UseBuiltIns {
    fn default() -> Self {
        UseBuiltIns::Bool(false)
    }
}

#[derive(Debug, FromVariant)]
enum Error {
    Io(io::Error),
    Var(env::VarError),
    WalkDir(walkdir::Error),
    Json(serde_json::Error),
}

fn load() -> Result<Vec<TestDescAndFn>, Error> {
    let mut tests = vec![];
    let mut dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR")?);
    dir.push("tests");
    dir.push("fixtures");

    for entry in WalkDir::new(dir) {
        let e = entry?;

        if e.metadata()?.is_file() {
            continue;
        }

        match e.path().join("input.mjs").metadata() {
            Ok(e) if e.is_file() => {}
            _ => continue,
        }

        let cfg: BabelOptions =
            serde_json::from_reader(File::open(e.path().join("options.json"))?)?;
        assert_eq!(cfg.presets.len(), 1);
        let cfg = cfg.presets.into_iter().map(|v| v.1).next().unwrap();
    }

    Ok(tests)
}

fn exec() {
    let res: Result<_, Error> = try {
        let mut query_js = PathBuf::from(env::var("CARGO_MANIFEST_DIR")?);
        query_js.push("tests");
        query_js.push("query.js");

        Command::new("node").arg(query_js).status()?
    };
}

#[test]
fn fixtures() {
    load().expect("failed to load fixtures");
}
