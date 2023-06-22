use super::{Bool, TemplateType, True, TryDefault};
use ast::Segment as ASeg;
use nom::{error::Error as NomError, Err as Nerr, IResult};
use thiserror::Error;

#[derive(Debug, Error)]
enum TemplateParseError {
    #[error("parser error: {0}")]
    NomErr(#[from] NomError<String>),
    #[error("template type '{0}' can only contain a primitive expression")]
    ComplexPrimitive(char),
    #[error("template type '{0}' is not allowed in this field")]
    InvalidTemplateType(char),
    #[error("unrecognized template type {0}")]
    UnrecognizedTemplateType(char),
    #[error("only primitive templatings are allowed to be nested")]
    NestedComplex,
}

impl<'a> From<NomError<&'a str>> for TemplateParseError {
    fn from(value: NomError<&'a str>) -> Self {
        let NomError { input, code } = value;
        Self::NomErr(NomError {
            input: input.to_owned(),
            code,
        })
    }
}

enum Segment<T: TemplateType, PO: Bool> {
    Literal(String),
    Env(String, T::EnvsAllowed),
    Var(String, T::VarsAllowed),
    Prov(String, T::ProvAllowed),
    Expr(Vec<Segment<T, True>>, (T::ProvAllowed, PO::Inverse)),
}

impl<T: TemplateType, PO: Bool> Segment<T, PO> {
    fn parse(input: &str) -> IResult<&str, Self, TemplateParseError> {
        use ast::TemplateSegment as ATem;
        ASeg::parse(input)
            .map_err(Nerr::convert)
            .and_then(|(input, s)| match s {
                ASeg::Literal(r) => Ok((input, Self::Literal(r.to_string()))),
                ASeg::Template(ATem { tag: 'e', inner }) => {
                    Self::prim_gen(Self::Env, 'e', input, inner)
                }
                ASeg::Template(ATem { tag: 'v', inner }) => {
                    Self::prim_gen(Self::Var, 'v', input, inner)
                }
                ASeg::Template(ATem { tag: 'p', inner }) => {
                    Self::prim_gen(Self::Prov, 'p', input, inner)
                }
                ASeg::Template(ATem { tag: 'x', inner }) => todo!(),
                ASeg::Template(ATem { tag, .. }) => {
                    Err(TemplateParseError::UnrecognizedTemplateType(tag)).map_err(Nerr::Error)
                }
            })
    }

    fn prim_gen<'a, F, D>(
        f: F,
        tag: char,
        input: &'a str,
        seg: Vec<ASeg>,
    ) -> IResult<&'a str, Self, TemplateParseError>
    where
        F: FnOnce(String, D) -> Self,
        D: TryDefault,
    {
        as_primitive(seg)
            .ok_or(TemplateParseError::ComplexPrimitive(tag))
            .and_then(|x| Ok((input, f(x, TryDefault::try_default().unwrap()))))
            .map_err(Nerr::Error)
            .map_err(Nerr::convert)
    }
}

fn as_primitive(seg: Vec<ASeg>) -> Option<String> {
    match seg[..] {
        [ASeg::Literal(r)] => Some(r.to_string()),
        _ => None,
    }
}

mod ast {
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_until1},
        character::complete::anychar,
        combinator::{rest, value},
        multi::many1,
        sequence::{delimited, separated_pair},
        IResult, Parser,
    };
    use std::fmt::{self, Display};

    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    pub enum Raw<'a> {
        Literal(&'a str),
        EscapedDollarSign,
    }

    impl<'a> Display for Raw<'a> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Self::Literal(s) => s,
                    Self::EscapedDollarSign => "$$",
                }
            )
        }
    }

    impl<'a> Raw<'a> {
        fn escaped(input: &str) -> IResult<&str, Self> {
            value(Self::EscapedDollarSign, tag("$$"))(input)
        }

        fn literal(input: &'a str) -> IResult<&str, Self> {
            alt((take_until1("$"), rest))
                .map(Self::Literal)
                .parse(input)
        }

        fn parse(input: &'a str) -> IResult<&str, Self> {
            alt((Self::escaped, Self::literal))(input)
        }
    }

    pub enum Segment<'a> {
        Literal(Raw<'a>),
        Template(TemplateSegment<'a>),
    }

    impl<'a> Segment<'a> {
        pub fn parse(input: &'a str) -> IResult<&str, Self> {
            alt((
                Raw::parse.map(Self::Literal),
                TemplateSegment::parse.map(Self::Template),
            ))(input)
        }
    }

    pub struct TemplateSegment<'a> {
        pub tag: char,
        pub inner: Vec<Segment<'a>>,
    }

    impl<'a> TemplateSegment<'a> {
        fn parse(input: &'a str) -> IResult<&str, Self> {
            delimited(
                tag("${"),
                separated_pair(anychar, tag(":"), many1(Segment::parse))
                    .map(|(tag, inner)| Self { tag, inner }),
                tag("}"),
            )(input)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::Raw;

        #[test]
        fn test_raws() {
            let input = "foo";
            let (rem, raw) = Raw::parse(input).unwrap();
            assert_eq!(rem, "");
            assert_eq!(raw, Raw::Literal("foo"));
            let input = "$$";
            let (rem, raw) = Raw::parse(input).unwrap();
            assert_eq!(rem, "");
            assert_eq!(raw, Raw::EscapedDollarSign);
            let input = "foo-${v:bar}";
            let (rem, raw) = Raw::parse(input).unwrap();
            assert_eq!(rem, "${v:bar}");
            assert_eq!(raw, Raw::Literal("foo-"));
        }
    }
}
