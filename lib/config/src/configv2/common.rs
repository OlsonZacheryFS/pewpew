use super::{
    templating::{Bool, False, Regular, Template, True},
    PropagateVars,
};
use derive_more::{Deref, DerefMut, From};
use serde::Deserialize;
use std::{convert::TryFrom, str::FromStr, time::Duration as SDur};
use thiserror::Error;

#[derive(Debug, Deserialize, Deref, PartialEq, Eq, From, DerefMut)]
pub struct Headers<VD: Bool>(
    #[serde(with = "tuple_vec_map")]
    #[serde(default = "Vec::new")]
    Vec<(String, Template<String, Regular, VD>)>,
);

impl<VD: Bool> Headers<VD> {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

impl<VD: Bool> Default for Headers<VD> {
    fn default() -> Self {
        Self(vec![])
    }
}

impl PropagateVars for Headers<False> {
    type Data<VD: Bool> = Headers<VD>;

    fn insert_vars(self, vars: &super::Vars<True>) -> Result<Self::Data<True>, super::VarsError> {
        self.0
            .into_iter()
            .map(|(n, h)| Ok((n, h.insert_vars(vars)?)))
            .collect::<Result<_, _>>()
            .map(Headers)
    }
}

/// Newtype wrapper around [`std::time::Duration`] that allows implementing the needed traits.
#[derive(Debug, PartialEq, Clone, Copy, Eq, Deref)]
pub struct Duration(SDur);

#[derive(Debug, Error, PartialEq, Eq)]
#[error("'{0}' is not a valid duration")]
pub struct DurationError(String);

impl FromStr for Duration {
    type Err = DurationError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        crate::shared::duration_from_string(s)
            .map(Self)
            .ok_or(DurationError(s.to_owned()))
    }
}

impl TryFrom<&str> for Duration {
    type Error = <Self as FromStr>::Err;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl Duration {
    pub fn from_secs(secs: u64) -> Self {
        Self(SDur::from_secs(secs))
    }
}

#[derive(Debug, Deserialize, PartialEq, Eq, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum ProviderSend {
    Block,
    Force,
    IfNotFull,
}

impl ProviderSend {
    pub fn is_block(&self) -> bool {
        matches!(self, Self::Block)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_yaml::from_str as from_yaml;

    #[test]
    fn basic_test_duration() {
        assert_eq!("1m".parse(), Ok(Duration::from_secs(60)));
        assert_eq!(
            "3h1m22s".parse(),
            Ok(Duration::from_secs(3 * 60 * 60 + 60 + 22))
        );
        assert_eq!("5hrs".parse(), Ok(Duration::from_secs(5 * 60 * 60)));
    }

    #[test]
    fn basic_test_provider_send() {
        // provider send
        let ps: ProviderSend = from_yaml("!block").unwrap();
        assert_eq!(ps, ProviderSend::Block);
        let ps: ProviderSend = from_yaml("!force").unwrap();
        assert_eq!(ps, ProviderSend::Force);
        let ps: ProviderSend = from_yaml("!if_not_full").unwrap();
        assert_eq!(ps, ProviderSend::IfNotFull);
    }

    #[test]
    fn headers_multi_define() {
        let input = r#"
        auth: null
        auth: foo
        "#;
        let headers = from_yaml::<Headers<True>>(input).unwrap();
        assert_eq!(headers.0.len(), 2);
        assert_eq!(headers.0[0].0, "auth");
        assert_eq!(headers.0[0].1.as_static().unwrap(), "null");
        assert_eq!(headers.0[1].0, "auth");
        assert_eq!(headers.0[1].1.as_static().unwrap(), "foo");
    }
}
