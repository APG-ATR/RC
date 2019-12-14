use crate::BrowserData;
use hashbrown::HashMap;
use once_cell::sync::Lazy;
use semver::Version;
use string_enum::StringEnum;

#[derive(Clone, Copy, PartialEq, Eq, StringEnum, Hash)]
pub(crate) enum Feature {
    /// `transform-template-literals`
    TemplateLiterals,

    /// `transform-literals`
    Literals,
}

pub(crate) static FEATURES: Lazy<HashMap<Feature, BrowserData<Option<Version>>>> =
    Lazy::new(|| {
        let map: HashMap<Feature, BrowserData<Option<Version>>> =
            serde_json::from_str(include_str!("transform_data.json"))
                .expect("failed to parse json");

        map
    });
