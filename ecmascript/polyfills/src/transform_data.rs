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

    /// `transform-function-name`
    FunctionName,

    /// `transform-arrow-functions`
    ArrowFunctions,

    /// `transform-block-scoped-functions`
    BlockScopedFunctions,

    /// `transform-classes`
    Classes,

    /// `transform-object-super`
    ObjectSuper,

    /// `transform-shorthand-properties`
    ShorthandProperties,

    /// `transform-duplicate-keys`
    DuplicateKeys,

    /// `transform-computed-properties`
    ComputedProperties,

    /// `transform-for-of`
    ForOf,

    /// `transform-sticky-regex`
    StickyRegex,

    /// `transform-dotall-regex`
    DotAllRegex,

    /// `transform-unicode-regex`
    UnicodeRegex,

    /// `transform-spread`
    Spread,

    /// `transform-parameters`
    Parameters,

    /// `transform-destructuring`
    Destructuring,

    /// `transform-block-scoping`
    BlockScoping,

    /// `transform-typeof-symbol`
    TypeOfSymbol,

    /// `transform-new-target`
    NewTarget,

    /// `transform-regenerator`
    Regenerator,

    /// `transform-exponentiation-operator`
    ExponentiationOperator,

    /// `transform-async-to-generator`
    AsyncToGenerator,

    /// `proposal-async-generator-functions`
    AsyncGeneratorFunctions,

    /// `proposal-object-rest-spread`
    ObjectRestSpread,

    /// `proposal-unicode-property-regex`
    UnicodePropertyRegex,

    /// `proposal-json-strings`
    JsonStrings,

    /// `proposal-optional-catch-binding`
    OptionalCatchBinding,

    /// `transform-named-capturing-groups-regex`
    NamedCapturingGroupsRegex,

    /// `transform-member-expression-literals`
    MemberExpressionLiterals,

    /// `transform-property-literals`
    PropertyLiterals,

    /// `transform-reserved-words`
    ReservedWords,
}

pub(crate) static FEATURES: Lazy<HashMap<Feature, BrowserData<Option<String>>>> = Lazy::new(|| {
    let map: HashMap<Feature, BrowserData<Option<String>>> =
        serde_json::from_str(include_str!("transform_data.json")).expect("failed to parse json");

    map.into_iter()
        .map(|(feature, data)| {
            // TODO: data: "42" -> "42.0.0"

            (feature, data)
        })
        .collect()
});
