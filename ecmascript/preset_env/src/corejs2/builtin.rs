use crate::{parse_version, BrowserData};
use hashbrown::HashMap;
use once_cell::sync::Lazy;
use semver::Version;
use swc_atoms::JsWord;

pub(crate) static BUILTINS: Lazy<HashMap<String, BrowserData<Option<Version>>>> = Lazy::new(|| {
    let map: HashMap<_, BrowserData<Option<String>>> =
        serde_json::from_str(include_str!("builtin.json")).expect("failed to parse json");

    map.into_iter()
        .map(|(feature, version)| {
            (
                feature,
                version.map_value(|version| version.map(|v| parse_version(&*v))),
            )
        })
        .collect()
});
