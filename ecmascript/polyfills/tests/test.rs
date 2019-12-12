#![feature(try_blocks)]

use std::{env, io, path::PathBuf, process::Command};
use swc_common::FromVariant;

#[derive(Debug, FromVariant)]
enum Error {
    Io(io::Error),
    Var(env::VarError),
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
fn fixtures() {}
