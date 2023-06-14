use super::super::templating::{Template, VarsOnly};
use super::{BufferLimit, ProviderSend};
use crate::configv2::templating::{Bool, False, True};
use crate::configv2::PropagateVars;
use serde::Deserialize;

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
pub struct FileProvider<VD: Bool = True> {
    pub path: Template<String, VarsOnly, VD>,
    #[serde(default)]
    pub repeat: bool,
    #[serde(default)]
    pub unique: bool,
    pub auto_return: Option<ProviderSend>,
    #[serde(default)]
    pub buffer: BufferLimit,
    #[serde(default)]
    pub format: FileReadFormat,
    #[serde(default)]
    pub random: bool,
}

impl PropagateVars for FileProvider<False> {
    type Residual = FileProvider<True>;

    fn insert_vars(
        self,
        vars: &crate::configv2::VarValue<True>,
    ) -> Result<Self::Residual, crate::configv2::VarsError> {
        let Self {
            path,
            repeat,
            unique,
            auto_return,
            buffer,
            format,
            random,
        } = self;

        Ok(FileProvider {
            path: path.insert_vars(vars)?,
            repeat,
            unique,
            auto_return,
            buffer,
            format,
            random,
        })
    }
}

/// How the data should be read from the file.
#[derive(Debug, Deserialize, PartialEq, Eq, Default, Clone)]
#[serde(rename_all = "snake_case")]
pub enum FileReadFormat {
    /// Read one line at a time, as either a string or a JSON object.
    /// Json objects that span mulitple lines are not supported in this format.
    #[default]
    Line,
    /// Read the file as a sequence of JSON objects, separated by either whitespace of
    /// self-delineation
    Json,
    /// Read the file as a CSV, with each line being a record, and the first line possibly being
    /// the headers.
    Csv(CsvParams),
}

#[derive(Debug, Deserialize, PartialEq, Eq, Default, Clone)]
pub struct CsvParams {
    // TODO: deserialize u8 from char
    pub comment: Option<u8>,
    // TODO: deserialize u8 from char
    pub delimiter: Option<u8>,
    #[serde(default = "default_double_quote")]
    pub double_quote: bool,
    // TODO: deserialize u8 from char
    pub escape: Option<u8>,
    #[serde(default)]
    pub headers: CsvHeaders,
    // TODO: deserialize u8 from char
    pub terminator: Option<u8>,
    // TODO: deserialize u8 from char
    pub quote: Option<u8>,
}

const fn default_double_quote() -> bool {
    true
}

/// Define what, if any, headers should be used for each CSV record.
#[derive(Deserialize, Debug, PartialEq, Eq, Clone)]
#[serde(untagged)]
pub enum CsvHeaders {
    /// Specify if the first row should be used as headers, or if no headers should be used.
    Use(bool),
    /// Provide header values directly.
    Provide(Vec<String>),
}

impl Default for CsvHeaders {
    fn default() -> Self {
        Self::Use(false)
    }
}

#[cfg(test)]
mod tests {
    use crate::configv2::templating::False;

    use super::*;
    use serde_yaml::from_str as from_yaml;

    #[test]
    fn test_csv_headers() {
        let ch = from_yaml::<CsvHeaders>("true").unwrap();
        assert_eq!(ch, CsvHeaders::Use(true));
        let ch = from_yaml::<CsvHeaders>("- hello\n- world").unwrap();
        assert_eq!(
            ch,
            CsvHeaders::Provide(vec!["hello".to_owned(), "world".to_owned()])
        );
    }

    #[test]
    fn test_file_read_format_basic() {
        let frf = from_yaml::<FileReadFormat>("!line").unwrap();
        assert_eq!(frf, FileReadFormat::Line);
        let frf = from_yaml::<FileReadFormat>("!json").unwrap();
        assert_eq!(frf, FileReadFormat::Json);
    }

    #[test]
    fn test_file_read_format_csv() {
        // defaults
        let frf = from_yaml::<FileReadFormat>("!csv").unwrap();
        let FileReadFormat::Csv (
            CsvParams {comment,
            delimiter,
            double_quote,
            escape,
            headers,
            terminator,
            quote,
        }) = frf else { panic!("was not csv") };
        assert_eq!(comment, None);
        assert_eq!(delimiter, None);
        assert_eq!(double_quote, true);
        assert_eq!(escape, None);
        assert_eq!(headers, CsvHeaders::Use(false));
        assert_eq!(terminator, None);
        assert_eq!(quote, None);

        // filled
        let frf = from_yaml::<FileReadFormat>(
            r##"
!csv
  comment: 65
  delimiter: ;
  double_quote: false
  escape: 10
  headers: true
  terminator: 55
  quote: 75
        "##,
        )
        .unwrap();
        let FileReadFormat::Csv (CsvParams{
            comment,
            delimiter,
            double_quote,
            escape,
            headers,
            terminator,
            quote,
        }) = frf else { panic!("was not csv") };
        assert_eq!(comment, Some(65));
        assert_eq!(delimiter, None);
        assert_eq!(double_quote, false);
        assert_eq!(escape, Some(10));
        assert_eq!(headers, CsvHeaders::Use(true));
        assert_eq!(terminator, Some(55));
        assert_eq!(quote, Some(75));

        // array headers
        let frf = from_yaml(
            r#"
!csv
  headers:
    - foo
    - bar
        "#,
        )
        .unwrap();
        let FileReadFormat::Csv(CsvParams {
            comment,
            delimiter,
            double_quote,
            escape,
            headers,
            terminator,
            quote,
        }) = frf else { panic!("was not csv") };
        assert_eq!(comment, None);
        assert_eq!(delimiter, None);
        assert_eq!(double_quote, true);
        assert_eq!(escape, None);
        assert_eq!(
            headers,
            CsvHeaders::Provide(vec!["foo".to_owned(), "bar".to_owned()])
        );
        assert_eq!(terminator, None);
        assert_eq!(quote, None);
    }

    #[test]
    fn test_file_provider() {
        static TEST1: &str = "path: !l file.txt";

        let FileProvider::<False> {
            path,
            repeat,
            unique,
            auto_return,
            buffer,
            format,
            random,
        } = from_yaml(TEST1).unwrap();
        assert_eq!(
            path,
            Template::Literal {
                value: "file.txt".to_owned()
            }
        );
        assert_eq!(repeat, false);
        assert_eq!(unique, false);
        assert_eq!(auto_return, None);
        assert_eq!(buffer, BufferLimit::Auto);
        assert_eq!(format, FileReadFormat::Line);
        assert_eq!(random, false);

        static TEST2: &str = r"
path: !l file2.txt
repeat: true
unique: true
auto_return: !if_not_full
buffer: 9987
format: !json
random: true
        ";

        let FileProvider::<False> {
            path,
            repeat,
            unique,
            auto_return,
            buffer,
            format,
            random,
        } = from_yaml(TEST2).unwrap();
        assert_eq!(
            path,
            Template::Literal {
                value: "file2.txt".to_owned()
            }
        );
        assert_eq!(repeat, true);
        assert_eq!(unique, true);
        assert_eq!(auto_return, Some(ProviderSend::IfNotFull));
        assert_eq!(buffer, BufferLimit::Limit(9987));
        assert_eq!(format, FileReadFormat::Json);
        assert_eq!(random, true);

        static TEST3: &str = r"
path: !l file3.csv
format: !csv
  headers:
    - foo
    - bar";

        let FileProvider::<False> {
            path,
            repeat,
            unique,
            auto_return,
            buffer,
            format,
            random,
        } = from_yaml(TEST3).unwrap();
        assert_eq!(
            path,
            Template::Literal {
                value: "file3.csv".to_owned()
            }
        );
        assert_eq!(repeat, false);
        assert_eq!(unique, false);
        assert_eq!(auto_return, None);
        assert_eq!(buffer, BufferLimit::Auto);
        assert_eq!(random, false);
        let FileReadFormat::Csv(CsvParams {
            comment,
            delimiter,
            double_quote,
            escape,
            headers,
            terminator,
            quote,
        }) = format else { panic!("was not csv") };
        assert_eq!(comment, None);
        assert_eq!(delimiter, None);
        assert_eq!(double_quote, true);
        assert_eq!(escape, None);
        assert_eq!(
            headers,
            CsvHeaders::Provide(vec!["foo".to_owned(), "bar".to_owned()])
        );
        assert_eq!(terminator, None);
        assert_eq!(quote, None);
    }
}
