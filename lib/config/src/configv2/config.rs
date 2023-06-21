#![allow(dead_code)]

use super::{
    common::{Duration, Headers},
    templating::{Bool, False, True},
    PropagateVars,
};
use serde::Deserialize;

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Config<VD: Bool = True> {
    pub client: Client<VD>,
    pub general: General,
}

impl PropagateVars for Config<False> {
    type Residual = Config<True>;

    fn insert_vars(self, vars: &super::VarValue<True>) -> Result<Self::Residual, super::VarsError> {
        Ok(Config {
            client: self.client.insert_vars(vars)?,
            general: self.general,
        })
    }
}

/// Customization Parameters for the HTTP client
#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Client<VD: Bool> {
    #[serde(default = "default_timeout")]
    pub request_timeout: Duration,
    #[serde(default = "Headers::new")]
    headers: Headers<VD>,
    #[serde(default = "default_keepalive")]
    pub keepalive: Duration,
}

impl PropagateVars for Client<False> {
    type Residual = Client<True>;

    fn insert_vars(self, vars: &super::VarValue<True>) -> Result<Self::Residual, super::VarsError> {
        let Self {
            request_timeout,
            headers,
            keepalive,
        } = self;
        Ok(Client {
            request_timeout,
            headers: headers.insert_vars(vars)?,
            keepalive,
        })
    }
}

#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct General {
    #[serde(default = "default_buffer_start_size")]
    pub auto_buffer_start_size: u64,
    #[serde(default = "default_bucket_size")]
    pub bucket_size: Duration,
    #[serde(default = "default_log_provider_stats")]
    pub log_provider_stats: bool,
    watch_transition_time: Option<Duration>,
}

fn default_timeout() -> Duration {
    Duration::from_secs(60)
}

fn default_keepalive() -> Duration {
    Duration::from_secs(90)
}

const fn default_buffer_start_size() -> u64 {
    5
}

fn default_bucket_size() -> Duration {
    Duration::from_secs(60)
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
        assert_eq!(request_timeout, Duration::from_secs(60));
        assert!(headers.is_empty());
        assert_eq!(keepalive, Duration::from_secs(90));

        static TEST2: &str = r#"
request_timeout: 23s
headers:
  one: !l two
keepalive: 19s
        "#;

        let Client::<False> {
            request_timeout,
            headers,
            keepalive,
        } = from_yaml(TEST2).unwrap();
        assert_eq!(request_timeout, Duration::from_secs(23));
        assert_eq!(headers.len(), 1);
        assert_eq!(headers[0].0, "one");
        assert_eq!(
            headers[0].1,
            Template::Literal {
                value: "two".to_owned()
            }
        );
        assert_eq!(keepalive, Duration::from_secs(19));
    }

    #[test]
    fn test_general() {
        static TEST1: &str = "";
        let General {
            auto_buffer_start_size,
            bucket_size,
            log_provider_stats,
            watch_transition_time,
        } = from_yaml(TEST1).unwrap();
        assert_eq!(auto_buffer_start_size, 5);
        assert_eq!(bucket_size, Duration::from_secs(60));
        assert_eq!(log_provider_stats, true);
        assert_eq!(watch_transition_time, None);

        static TEST2: &str = r#"
auto_buffer_start_size: 100
bucket_size: 2m
log_provider_stats: false
watch_transition_time: 23s
        "#;
        let General {
            auto_buffer_start_size,
            bucket_size,
            log_provider_stats,
            watch_transition_time,
        } = from_yaml(TEST2).unwrap();
        assert_eq!(auto_buffer_start_size, 100);
        assert_eq!(bucket_size, Duration::from_secs(120));
        assert_eq!(log_provider_stats, false);
        assert_eq!(watch_transition_time, Some(Duration::from_secs(23)));
    }

    #[test]
    fn test_config() {
        static TEST1: &str = "client: {}\ngeneral: {}";
        let Config { client, general } = from_yaml(TEST1).unwrap();
        assert_eq!(client, from_yaml::<Client<False>>("").unwrap());
        assert_eq!(general, from_yaml::<General>("").unwrap());

        static TEST2: &str = r#"
client:
  request_timeout: 89s
general:
  bucket_size: 1 hour
        "#;
        let Config::<False> { client, general } = from_yaml(TEST2).unwrap();
        assert_eq!(client.request_timeout, Duration::from_secs(89));
        assert_eq!(general.bucket_size, Duration::from_secs(3600));
    }
}
