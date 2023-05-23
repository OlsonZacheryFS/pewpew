#![allow(dead_code)]

use super::{
    common::{Duration, Headers},
    templating::{Bool, False, Template, True, VarsOnly},
    PropagateVars,
};
use serde::Deserialize;

#[derive(Deserialize, Debug, PartialEq, Eq)]
pub struct Config<VD: Bool = True> {
    pub client: Client<VD>,
    #[serde(default = "Default::default")]
    pub general: General<VD>,
}

impl PropagateVars for Config<False> {
    type Data<VD: Bool> = Config<VD>;

    fn insert_vars(self, vars: &super::Vars<True>) -> Result<Self::Data<True>, super::VarsError> {
        Ok(Config {
            client: self.client.insert_vars(vars)?,
            general: self.general.insert_vars(vars)?,
        })
    }
}

/// Customization Parameters for the HTTP client
#[derive(Deserialize, Debug, PartialEq, Eq)]
pub struct Client<VD: Bool> {
    #[serde(default = "default_timeout")]
    pub request_timeout: Template<Duration, VarsOnly, VD>,
    #[serde(default = "Headers::new")]
    pub(crate) headers: Headers<VD>,
    #[serde(default = "default_keepalive")]
    pub keepalive: Template<Duration, VarsOnly, VD>,
}

impl PropagateVars for Client<False> {
    type Data<VD: Bool> = Client<VD>;

    fn insert_vars(self, vars: &super::Vars<True>) -> Result<Self::Data<True>, super::VarsError> {
        let Self {
            request_timeout,
            headers,
            keepalive,
        } = self;
        Ok(Client {
            request_timeout: request_timeout.insert_vars(vars)?,
            headers: headers.insert_vars(vars)?,
            keepalive: keepalive.insert_vars(vars)?,
        })
    }
}

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct General<VD: Bool> {
    #[serde(default = "default_buffer_start_size")]
    pub auto_buffer_start_size: u64,
    #[serde(default = "default_bucket_size")]
    pub bucket_size: Template<Duration, VarsOnly, VD>,
    #[serde(default = "default_log_provider_stats")]
    pub log_provider_stats: bool,
    watch_transition_time: Option<Template<Duration, VarsOnly, VD>>,
}

impl<B: Bool> Default for General<B> {
    fn default() -> Self {
        Self {
            auto_buffer_start_size: default_buffer_start_size(),
            bucket_size: default_bucket_size(),
            log_provider_stats: default_log_provider_stats(),
            watch_transition_time: None,
        }
    }
}

impl PropagateVars for General<False> {
    type Data<VD: Bool> = General<VD>;

    fn insert_vars(
        self,
        vars: &super::Vars<True>,
    ) -> Result<Self::Data<True>, crate::error::VarsError> {
        Ok(General {
            auto_buffer_start_size: self.auto_buffer_start_size,
            bucket_size: self.bucket_size.insert_vars(vars)?,
            log_provider_stats: self.log_provider_stats,
            watch_transition_time: self
                .watch_transition_time
                .map(|w| w.insert_vars(vars))
                .transpose()?,
        })
    }
}

fn default_timeout<VD: Bool>() -> Template<Duration, VarsOnly, VD> {
    Template::new_literal(Duration::from_secs(60))
}

fn default_keepalive<VD: Bool>() -> Template<Duration, VarsOnly, VD> {
    Template::new_literal(Duration::from_secs(90))
}

const fn default_buffer_start_size() -> u64 {
    5
}

fn default_bucket_size<VD: Bool>() -> Template<Duration, VarsOnly, VD> {
    Template::new_literal(Duration::from_secs(60))
}

const fn default_log_provider_stats() -> bool {
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::configv2::templating::{False, Template};
    use serde_yaml::from_str as from_yaml;

    #[test]
    fn test_client() {
        static TEST1: &str = "";
        let Client::<False> {
            request_timeout,
            headers,
            keepalive,
        } = from_yaml(TEST1).unwrap();
        assert_eq!(
            request_timeout,
            Template::new_literal(Duration::from_secs(60))
        );
        assert!(headers.is_empty());
        assert_eq!(keepalive, Template::new_literal(Duration::from_secs(90)));

        static TEST2: &str = r#"
request_timeout: 23s
headers:
  one: two
keepalive: 19s
        "#;

        let Client::<False> {
            request_timeout,
            headers,
            keepalive,
        } = from_yaml(TEST2).unwrap();
        assert_eq!(
            request_timeout,
            Template::new_literal(Duration::from_secs(23))
        );
        assert_eq!(headers.len(), 1);
        assert_eq!(headers[0].0, "one");
        assert_eq!(
            headers[0].1,
            Template::Literal {
                value: "two".to_owned()
            }
        );
        assert_eq!(keepalive, Template::new_literal(Duration::from_secs(19)));
    }

    #[test]
    fn test_general() {
        static TEST1: &str = "";
        let General::<False> {
            auto_buffer_start_size,
            bucket_size,
            log_provider_stats,
            watch_transition_time,
        } = from_yaml(TEST1).unwrap();
        assert_eq!(auto_buffer_start_size, 5);
        assert_eq!(bucket_size, Template::new_literal(Duration::from_secs(60)));
        assert_eq!(log_provider_stats, true);
        assert_eq!(watch_transition_time, None);

        static TEST2: &str = r#"
auto_buffer_start_size: 100
bucket_size: 2m
log_provider_stats: false
watch_transition_time: 23s
        "#;
        let General::<False> {
            auto_buffer_start_size,
            bucket_size,
            log_provider_stats,
            watch_transition_time,
        } = from_yaml(TEST2).unwrap();
        assert_eq!(auto_buffer_start_size, 100);
        assert_eq!(bucket_size, Template::new_literal(Duration::from_secs(120)));
        assert_eq!(log_provider_stats, false);
        assert_eq!(
            watch_transition_time,
            Some(Template::new_literal(Duration::from_secs(23)))
        );
    }

    #[test]
    fn test_config() {
        static TEST1: &str = "client: {}\ngeneral: {}";
        let Config { client, general } = from_yaml(TEST1).unwrap();
        assert_eq!(client, from_yaml::<Client<False>>("").unwrap());
        assert_eq!(general, from_yaml::<General<False>>("").unwrap());

        static TEST2: &str = r#"
client:
  request_timeout: 89s
general:
  bucket_size: 1 hour
        "#;
        let Config::<False> { client, general } = from_yaml(TEST2).unwrap();
        assert_eq!(
            client.request_timeout,
            Template::new_literal(Duration::from_secs(89))
        );
        assert_eq!(
            general.bucket_size,
            Template::new_literal(Duration::from_secs(3600))
        );
    }
}
