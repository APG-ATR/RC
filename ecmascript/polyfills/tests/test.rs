#![feature(box_syntax)]
#![feature(try_blocks)]
#![feature(test)]

extern crate test;

use serde::Deserialize;
use serde_json::Value;
use std::{
    collections::HashMap,
    env,
    fs::File,
    io,
    path::PathBuf,
    process::{Command, Stdio},
};
use swc_common::FromVariant;
use test::{test_main, ShouldPanic, TestDesc, TestDescAndFn, TestFn, TestName, TestType};
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
    pub shipped_proposals: bool,

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
    Msg(String),
}

fn load() -> Result<Vec<TestDescAndFn>, Error> {
    let mut tests = vec![];
    let mut dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR")?);
    dir.push("tests");
    dir.push("fixtures");

    for entry in WalkDir::new(&dir) {
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

        let file = e.path().join("input.mjs");
        tests.push(TestDescAndFn {
            desc: TestDesc {
                test_type: TestType::IntegrationTest,
                name: TestName::DynTestName(format!(
                    "{}",
                    e.path()
                        .strip_prefix(&dir)
                        .expect("failed to strip prefix")
                        .display()
                )),
                ignore: false,
                allow_fail: false,
                should_panic: ShouldPanic::No,
            },
            testfn: TestFn::DynTestFn(box move || {
                //
                exec(cfg, file).expect("failed to run test")
            }),
        });
    }

    Ok(tests)
}

fn exec(c: PresetConfig, src: PathBuf) -> Result<(), Error> {
    let output = {
        let mut qjs = PathBuf::from(env::var("CARGO_MANIFEST_DIR")?);
        qjs.push("tests");
        qjs.push("query.js");

        let output = Command::new("node")
            .arg(&qjs)
            .arg(serde_json::to_string(&c.targets)?)
            .output()?;

        println!(
            "{}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );

        if !output.status.success() {
            return Err(Error::Msg(format!("query.js: Status {:?}", output.status,)));
        }

        output.stdout
    };
    Ok(())
}

#[test]
fn fixtures() {
    let tests = load().expect("failed to load fixtures");

    let args: Vec<_> = env::args().collect();
    test_main(&args, tests, Some(test::Options::new()));
}
