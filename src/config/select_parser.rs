use super::{
    EndpointProvidesPreProcessed,
    EndpointProvidesSendOptions,
};
use crate::template::{textify, TextifyReturn, TextifyReturnFn, json_value_to_string};
use crate::util::{Either, Either3, parse_provider_name};

use handlebars::Handlebars;
use itertools::Itertools;
use pest::{
    iterators::{Pairs, Pair},
    Parser as PestParser,
};
use pest_derive::Parser;
use serde_json as json;

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    iter,
    sync::Arc,
};

#[derive(Clone)]
enum FunctionArg {
    FunctionCall(FunctionCall),
    Value(Value),
}

#[derive(Clone)]
enum FunctionCall {
    JsonPath(Arc<jsonpath::Selector>),
    Repeat(usize),
}

impl FunctionCall {
    fn new (
            ident: &str, args: &[FunctionArg],
            providers: &mut BTreeSet<String>,
            static_providers: &BTreeMap<String, json::Value>
        ) -> Either<Self, json::Value>
    {
        if ident == "json_path" {
            match (args.len(), args.first()) {
                // TODO: if there's ever another function which returns a string, modify this to allow a nested function
                (1, Some(FunctionArg::Value(Value::Json(false, json::Value::String(json_path))))) => {
                    let provider = parse_provider_name(&json_path);
                    // jsonpath requires the query to start with `$.`, so add it in
                    let json_path = format!("$.{}", json_path);
                    let json_path = jsonpath::Selector::new(&json_path)
                         .unwrap_or_else(|e| panic!("invalid json path query, {}\n{:?}", json_path, e));
                    let ret = if let Some(v) = static_providers.get(provider) {
                        let r = json_path.find(v);
                        Either::B(json::Value::Array(r.cloned().collect()))
                    } else {
                        Either::A(FunctionCall::JsonPath(Arc::new(json_path)))
                    };
                    providers.insert(provider.into());
                    ret
                },
                _ => panic!("invalid arguments for json_path")
            }
        } else if ident == "repeat" {
            match (args.len(), args.first()) {
                (1, Some(FunctionArg::Value(Value::Json(false, json::Value::Number(n))))) if n.is_u64() => {
                    Either::A(FunctionCall::Repeat(n.as_u64().unwrap() as usize))
                },
                 _ => panic!("invalid arguments for repeat")
            }
        } else {
            panic!("unknown function reference `{}`", ident);
        }
    }

    fn evaluate<'a>(&self, d: &'a json::Value) -> impl Iterator<Item=json::Value> + Clone {
        match &self {
            FunctionCall::JsonPath(jp) => {
                let result = jp.find(d);
                let v: Vec<_> = result.cloned().collect();
                Either::A(v.into_iter())
            },
            FunctionCall::Repeat(n) => 
                Either::B(iter::repeat(json::Value::Null).take(*n)),
        }
    }
}

fn index_json<'a>(json: &'a json::Value, index: Either<&JsonPathSegment, &str>) -> Cow<'a, json::Value> {
    #[allow(unused_assignments)]
    let mut holder = None;
    let str_or_number = match index {
        Either::A(jps) => {
            match jps.evaluate(json) {
                Either::A(s) => {
                    holder = Some(s);
                    Either::A(holder.as_ref().unwrap().as_str())
                },
                Either::B(n) => Either::B(n),
            }
        },
        Either::B(s) => Either::A(s),
    };
    let o = match (json, str_or_number) {
        (json::Value::Object(m), Either::A(s)) => m.get(s),
        (json::Value::Array(a), Either::B(n)) => a.get(n),
        (json::Value::Array(a), Either::A(s)) if s == "length" =>
            return Cow::Owned((a.len() as u64).into()),
        _ => panic!("cannot index into json {}", json)
    };
    Cow::Borrowed(o.unwrap_or(&json::Value::Null))
}

fn index_json2<'a>(mut json: &'a json::Value, indexes: &[JsonPathSegment]) -> json::Value {
    for (i, index) in indexes.iter().enumerate() {
        let o = match (json, index.evaluate(json)) {
            (json::Value::Object(m), Either::A(ref s)) => m.get(s),
            (json::Value::Array(a), Either::B(n)) => a.get(n),
            (json::Value::Array(a), Either::A(ref s)) if s == "length" => {
                let ret = (a.len() as u64).into();
                if i != indexes.len() - 1 {
                    panic!("cannot index into json {}", ret)
                }
                return ret
            },
            _ => panic!("cannot index into json {}", json),
        };
        json = o.unwrap_or(&json::Value::Null)
    }
    json.clone()
}

#[derive(Clone)]
struct JsonPath {
    start: JsonPathStart,
    rest: Vec<JsonPathSegment>,
}

impl JsonPath {
    fn evaluate<'a>(&self, d: &'a json::Value) -> impl Iterator<Item=json::Value> + Clone {
        match &self.start {
            JsonPathStart::FunctionCall(fnc) => {
                let rest = self.rest.clone();
                Either::A(
                    fnc.evaluate(d).map(move |j| index_json2(&j, &rest))
                )
            },
            JsonPathStart::JsonIdent(s) => {
                let j = index_json(d, Either::B(s));
                Either::B(
                    iter::once(index_json2(&j, &self.rest))
                )
            },
            JsonPathStart::Value(v) => {
                Either::B(iter::once(v.clone()))
            }
        }
    }
}

fn bool_value(json: &json::Value) -> bool {
    match json {
        json::Value::Null => false,
        json::Value::Bool(b) => *b,
        json::Value::Number(n) if n.is_i64() => n.as_i64().unwrap() != 0,
        json::Value::Number(n) if n.is_u64() => n.as_u64().unwrap() != 0,
        json::Value::Number(n) if n.is_f64() => n.as_f64().unwrap() != 0f64,
        json::Value::Number(_) => unreachable!("number that is not an integer or float"),
        json::Value::String(s) => !s.is_empty(),
        json::Value::Object(_) | json::Value::Array(_) => true,
    }
}

fn f64_value(json: &json::Value) -> f64 {
    if let Some(f) = json.as_f64() {
        f
    } else {
        std::f64::NAN
    }
}

type Not = bool;

#[derive(Clone)]
enum Value {
    JsonPath(Not, JsonPath),
    Json(Not, json::Value),
    Template(Not, Arc<TextifyReturnFn>),
}

impl Value {
    fn from_string(
            s: String,
            not: bool,
            handlebars: Arc<Handlebars>,
            providers: &mut BTreeSet<String>,
            static_providers: &BTreeMap<String, json::Value>
        ) -> Self
    {
        match textify(s, handlebars, providers, static_providers) {
            TextifyReturn::Trf(t) => Value::Template(not, t.into()),
            TextifyReturn::String(s) => Value::Json(not, s.into())
        }
    }

    fn evaluate<'a, 'b: 'a>(&'b self, d: &'a json::Value) -> Cow<'a, json::Value> {
        let (not, v) = match self {
            Value::JsonPath(not, path) => {
                let mut v: Vec<_> = path.evaluate(d).collect();
                let c = if v.is_empty() {
                    unreachable!("path should never return no elements");
                } else if v.len() == 1 {
                    Cow::Owned(v.pop().unwrap())
                } else {
                    Cow::Owned(json::Value::Array(v))
                };
                (*not, c)
            },
            Value::Json(not, value) => (*not, Cow::Borrowed(value)),
            Value::Template(not, t) => (*not, Cow::Owned(t(d)))
        };
        if not {
            Cow::Owned(json::Value::Bool(!bool_value(&v)))
        } else {
            v
        }
    }

    fn evaluate_as_iter<'a>(&self, d: &'a json::Value) -> impl Iterator<Item=json::Value> + Clone {
        match self {
            Value::JsonPath(not, path) => {
                if *not {
                    Either3::C(iter::once(false.into()))
                } else {
                    Either3::A(
                        path.evaluate(d)
                            .map(|v| {
                                if let json::Value::Array(v) = v {
                                    Either::A(v.into_iter())
                                } else {
                                    Either::B(iter::once(v))
                                }
                            })
                            .flatten()
                    )
                }
            },
            _ => {
                let value = self.evaluate(d).into_owned();
                match value {
                    json::Value::Array(v) => Either3::B(v.into_iter()),
                    _ => Either3::C(iter::once(value)),
                }
            }
        }
    }
}

#[derive(Clone)]
enum JsonPathSegment {
    Number(usize),
    String(String),
    Template(Arc<TextifyReturnFn>)
}

impl JsonPathSegment {
    fn from_string(
            s: String,
            handlebars: Arc<Handlebars>,
            providers: &mut BTreeSet<String>,
            static_providers: &BTreeMap<String, json::Value>
        ) -> Self
    {
        match textify(s, handlebars, providers, static_providers) {
            TextifyReturn::Trf(t) => JsonPathSegment::Template(t.into()),
            TextifyReturn::String(s) => JsonPathSegment::String(s)
        }
    }

    fn evaluate(&self, d: &json::Value) -> Either<String, usize> {
        match self {
            JsonPathSegment::Number(n) => Either::B(*n),
            JsonPathSegment::String(s) => Either::A(s.clone()),
            JsonPathSegment::Template(t) => Either::A(json_value_to_string(&t(d)))
        }
    }
}

#[derive(Clone)]
enum JsonPathStart {
    FunctionCall(FunctionCall),
    JsonIdent(String),
    Value(json::Value),
}

#[derive(Clone, PartialEq)]
enum Combiner {
    And,
    Or,
}

#[derive(Clone)]
enum Operator {
    Eq,
    Gt,
    Gte,
    Lt,
    Lte,
    Ne,
}

#[derive(Clone)]
enum Expression {
    Complex(ComplexExpression),
    Simple(SimpleExpression),
}

impl Expression {
    fn execute<'a>(&self, d: &'a json::Value) -> bool {
        match self {
            Expression::Complex(c) => c.execute(d),
            Expression::Simple(s) => s.execute(d),
        }
    }
}

#[derive(Clone)]
struct ComplexExpression {
    combiner: Combiner,
    pieces: Vec<Expression>,
}

impl ComplexExpression {
    fn execute(&self, d: &json::Value) -> bool {
        match self.combiner {
            Combiner::And => self.pieces.iter().all(|e| e.execute(d)),
            Combiner::Or => self.pieces.iter().any(|e| e.execute(d)),
        }
    }
}

#[derive(Clone)]
struct SimpleExpression {
    lhs: Value,
    rest: Option<(Operator, Value)>,
}

impl SimpleExpression {
    fn execute(&self, d: &json::Value) -> bool {
        let left = self.lhs.evaluate(d);
        if let Some((operator, right_value)) = &self.rest {
            let right = right_value.evaluate(d);
            match operator {
                Operator::Eq => left.eq(&right),
                Operator::Gt => f64_value(&left) > f64_value(&right),
                Operator::Gte => f64_value(&left) >= f64_value(&right),
                Operator::Lt => f64_value(&left) < f64_value(&right),
                Operator::Lte => f64_value(&left) <= f64_value(&right),
                Operator::Ne => left.ne(&right),
            }
        } else {
            bool_value(&left)
        }
    }
}

#[derive(Clone)]
enum ParsedSelect {
    Null,
    Bool(bool),
    Number(json::Number),
    Value(Value),
    Array(Vec<ParsedSelect>),
    Object(Vec<(String, ParsedSelect)>),
}

impl ParsedSelect {
    fn evaluate(&self, d: &json::Value) -> json::Value {
        match self {
            ParsedSelect::Null => json::Value::Null,
            ParsedSelect::Bool(b) => json::Value::Bool(*b),
            ParsedSelect::Number(n) => json::Value::Number(n.clone()),
            ParsedSelect::Value(v) => v.evaluate(d).into_owned(),
            ParsedSelect::Array(v) => {
                let v = v.iter().map(|p| p.evaluate(d))
                    .collect();
                json::Value::Array(v)
            },
            ParsedSelect::Object(v) => {
                let m = v.iter().map(|(k, v)| (k.clone(), v.evaluate(d)))
                    .collect();
                json::Value::Object(m)
            },
        }
    }
}

pub const REQUEST_STARTLINE: u16 = 0b000_000_100;
pub const REQUEST_HEADERS: u16 = 0b000_000_010;
pub const REQUEST_BODY: u16 = 0b000_000_001;
const REQUEST_ALL: u16 = REQUEST_STARTLINE | REQUEST_HEADERS | REQUEST_BODY;
pub const RESPONSE_STARTLINE: u16 = 0b000_100_000;
pub const RESPONSE_HEADERS: u16 = 0b000_010_000;
pub const RESPONSE_BODY: u16 = 0b000_001_000;
const RESPONSE_ALL: u16 = RESPONSE_STARTLINE | RESPONSE_HEADERS | RESPONSE_BODY;
const FOR_EACH: u16 = 0b001_000_000;
pub const STATS: u16 = 0b010_000_000;
pub const REQUEST_URL: u16 = 0b100_000_000;

#[derive(Clone, Parser)]
#[grammar = "config/select.pest"]
pub struct Select {
    join: Vec<Value>,
    providers: BTreeSet<String>,
    special_providers: u16,
    send_behavior: EndpointProvidesSendOptions,
    select: ParsedSelect,
    where_clause: Option<ComplexExpression>,
    where_clause_special_providers: u16,
}

fn providers_helper(incoming: &mut BTreeSet<String>, bitwise: &mut u16) {
    let previous = std::mem::replace(incoming, Default::default());
    for provider in previous.into_iter() {
        match provider.as_ref() {
            "request.start-line" => *bitwise |= REQUEST_STARTLINE,
            "request.headers" => *bitwise |= REQUEST_HEADERS,
            "request.body" => *bitwise |= REQUEST_BODY,
            "request.method" => (),
            "request.url" => *bitwise |= REQUEST_URL,
            "request" => *bitwise |= REQUEST_ALL,
            "response.start-line" => *bitwise |= RESPONSE_STARTLINE,
            "response.headers" => *bitwise |= RESPONSE_HEADERS,
            "response.body" => *bitwise |= RESPONSE_BODY,
            "response" => *bitwise |= RESPONSE_ALL,
            "response.status" => (),
            "stats" => *bitwise |= STATS,
            "for_each" => *bitwise |= FOR_EACH,
            _ => {
                incoming.insert(provider);
            },
        }
    }
}

impl Select {
    pub fn new(
            provides: EndpointProvidesPreProcessed,
            handlebars: &Arc<Handlebars>,
            static_providers: &BTreeMap<String, json::Value>
        ) -> Self
    {
        let mut providers = BTreeSet::new();
        let mut special_providers = 0;
        let join: Vec<_> = provides.for_each.iter().map(|s| {
            let pairs = Select::parse(Rule::value_entry, s).unwrap();
            let v = parse_value(pairs, handlebars, &mut providers, static_providers);
            if providers.contains("for_each") {
                panic!("cannot reference `for_each` from within `for_each`");
            }
            v
        }).collect();
        let mut where_clause_special_providers = 0;
        let where_clause = provides.where_clause.as_ref().map(|s| {
            let mut providers2 = BTreeSet::new();
            let pairs = Select::parse(Rule::where_entry, s).unwrap();
            let ce = parse_complex_expression(pairs, handlebars, &mut providers2, static_providers);
            providers_helper(&mut providers2, &mut where_clause_special_providers);
            providers.extend(providers2);
            ce
        });
        special_providers |= where_clause_special_providers;
        let select = parse_select(provides.select, handlebars, &mut providers, static_providers);
        providers_helper(&mut providers, &mut special_providers);
        Select {
            join,
            providers,
            special_providers,
            select,
            send_behavior: provides.send,
            where_clause,
            where_clause_special_providers,
        }
    }

    pub fn get_providers(&self) -> &BTreeSet<String> {
        &self.providers
    }

    pub fn get_special_providers(&self) -> u16 {
        self.special_providers
    }

    pub fn get_send_behavior(&self) -> &EndpointProvidesSendOptions {
        &self.send_behavior
    }

    pub fn get_where_clause_special_providers(&self) -> u16 {
        self.where_clause_special_providers
    }

    pub fn execute_where(&self, d: &json::Value) -> bool {
        self.where_clause.as_ref().map(|wc| wc.execute(d))
            .unwrap_or(true)
    }

    pub fn as_iter(&self, mut d: json::Value) -> impl Iterator<Item=json::Value> + Clone {
        if self.join.is_empty() {
            if let Some(wc) = &self.where_clause {
                if wc.execute(&d) {
                    Either3::A(iter::once(self.select.evaluate(&d)))
                } else {
                    Either3::B(iter::empty())
                }
            } else {
                Either3::A(iter::once(self.select.evaluate(&d)))
            }
        } else {
            let references_for_each = self.special_providers & FOR_EACH != 0;
            let where_clause = self.where_clause.clone();
            let select = self.select.clone();
            Either3::C(self.join.iter()
                .map(|v| v.evaluate_as_iter(&d))
                .multi_cartesian_product()
                .filter_map(move |v| {
                    if references_for_each {
                        d = d.clone();
                        d.as_object_mut().unwrap().insert("for_each".to_string(), json::Value::Array(v));
                    }
                    if let Some(wc) = where_clause.clone() {
                        if wc.execute(&d) {
                            Some(select.evaluate(&d))
                        } else {
                            None
                        }
                    } else {
                        Some(select.evaluate(&d))
                    }
                })
            )
        }
    }
}

fn parse_select(
        select: json::Value,
        handlebars: &Arc<Handlebars>,
        providers: &mut BTreeSet<String>,
        static_providers: &BTreeMap<String, json::Value>
    ) -> ParsedSelect
{
    match select {
        json::Value::Null => ParsedSelect::Null,
        json::Value::Bool(b) => ParsedSelect::Bool(b),
        json::Value::Number(n) => ParsedSelect::Number(n),
        json::Value::String(s) => {
            let pairs = Select::parse(Rule::value_entry, &s).unwrap();
            let value = parse_value(pairs, handlebars, providers, static_providers);
            ParsedSelect::Value(value)
        },
        json::Value::Array(a) => {
            let new = a.into_iter().map(|v| parse_select(v, handlebars, providers, static_providers))
                .collect();
            ParsedSelect::Array(new)
        },
        json::Value::Object(m) => {
            let new = m.into_iter().map(|(k, v)| (k, parse_select(v, handlebars, providers, static_providers))).collect();
            ParsedSelect::Object(new)
        }
    }
}

fn parse_function_call(
        pair: Pair<Rule>,
        handlebars: &Arc<Handlebars>,
        providers: &mut BTreeSet<String>,
        static_providers: &BTreeMap<String, json::Value>
    ) -> Either<FunctionCall, json::Value>
{
    let mut ident = None;
    let mut args = Vec::new();
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::function_ident => {
                ident = Some(pair.as_str());
            },
            Rule::function_call => {
                match parse_function_call(pair, handlebars, providers, static_providers) {
                    Either::A(fc) => args.push(FunctionArg::FunctionCall(fc)),
                    Either::B(v) => args.push(FunctionArg::Value(Value::Json(false, v)))
                }
            },
            Rule::value => {
                args.push(FunctionArg::Value(parse_value(pair.into_inner(), handlebars, providers, static_providers)));
            },
            r => unreachable!("unexpected rule for function call, `{:?}`", r)
        }
    }
    FunctionCall::new(ident.unwrap(), &args, providers, static_providers)
}

fn parse_indexed_property(
        pair: Pair<Rule>,
        handlebars: &Arc<Handlebars>,
        providers: &mut BTreeSet<String>,
        static_providers: &BTreeMap<String, json::Value>
    ) -> JsonPathSegment
{
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::string => JsonPathSegment::from_string(pair.as_str().into(), handlebars.clone(), providers, static_providers),
        Rule::integer => JsonPathSegment::Number(pair.as_str().parse().unwrap()),
        r => unreachable!("unexpected rule for path segment, `{:?}`", r)
    }
}

fn parse_json_path(
        pair: Pair<Rule>,
        handlebars: &Arc<Handlebars>,
        providers: &mut BTreeSet<String>,
        static_providers: &BTreeMap<String, json::Value>
    ) -> JsonPath
{
    let mut start = None;
    let mut rest = Vec::new();
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::function_call => {
                if start.is_none() {
                    let jps = match parse_function_call(pair, handlebars, providers, static_providers) {
                        Either::A(fc) => JsonPathStart::FunctionCall(fc),
                        Either::B(v) => JsonPathStart::Value(v),
                    };
                    start = Some(jps);
                } else {
                    unreachable!("encountered unexpected function call");
                }
            },
            Rule::json_ident => {
                let s: String = pair.as_str().into();
                if start.is_none() {
                    start = Some(JsonPathStart::JsonIdent(s));
                } else {
                    rest.push(
                        JsonPathSegment::from_string(s, handlebars.clone(), providers, static_providers)
                    );
                }
            },
            Rule::indexed_property => {
                if start.is_none() {
                    unreachable!("encountered unexpected indexed property");
                } else {
                    rest.push(parse_indexed_property(pair, handlebars, providers, static_providers));
                }
            },
            r => unreachable!("unexpected rule for json path, `{:?}`", r)
        }
    }
    let start = start.unwrap();
    if let JsonPathStart::JsonIdent(start) = &start {
        match (&start, rest.first()) {
            (start, Some(JsonPathSegment::String(next))) if start.as_str() == "request" || start.as_str() == "response" =>
                providers.insert(format!("{}.{}", start, next)),
            _ => providers.insert(start.clone()),
        };
    }
    JsonPath { start, rest }
}

fn parse_value(
        pairs: Pairs<Rule>,
        handlebars: &Arc<Handlebars>,
        providers: &mut BTreeSet<String>,
        static_providers: &BTreeMap<String, json::Value>
    ) -> Value
{
    let mut not = false;
    for pair in pairs {
        match pair.as_rule() {
            Rule::not => {
                not = true;
            },
            Rule::boolean => {
                let b = match pair.as_str() {
                    "true" => true,
                    "false" => false,
                    s => unreachable!("unexpected boolean value, `{}`", s),
                };
                return Value::Json(not, b.into())
            },
            Rule::null =>
                return Value::Json(not, json::Value::Null),
            Rule::json_path =>
                return Value::JsonPath(not, parse_json_path(pair, handlebars, providers, static_providers)),
            Rule::string => 
                return Value::from_string(pair.as_str().into(), not, handlebars.clone(), providers, static_providers),
            Rule::integer | Rule::decimal =>
                return Value::Json(
                    not,
                    json::Value::Number(std::str::FromStr::from_str(pair.as_str()).unwrap())
                ),
            Rule::value =>
                return parse_value(pair.into_inner(), handlebars, providers, static_providers),
            r => unreachable!("unexpected rule for value, `{:?}`", r)
        }
    }
    unreachable!("unexpectedly reached end of function in parse_value")
}

fn parse_simple_expression(
        pair: Pair<Rule>,
        handlebars: &Arc<Handlebars>,
        providers: &mut BTreeSet<String>,
        static_providers: &BTreeMap<String, json::Value>
    ) -> SimpleExpression
{
    let mut lhs = None;
    let mut operator = None;
    let mut rhs = None;
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::value => {
                let v = Some(parse_value(pair.into_inner(), handlebars, providers, static_providers));
                if lhs.is_none() {
                    lhs = v;
                } else {
                    rhs = v;
                }
            },
            Rule::operator => {
                let o = match pair.as_str() {
                    "==" => Operator::Eq,
                    "!=" => Operator::Ne,
                    ">=" => Operator::Gte,
                    "<=" => Operator::Lte,
                    ">" => Operator::Gt,
                    "<" => Operator::Lt,
                    o => unreachable!("unexpected operator, `{:?}`", o),
                };
                operator = Some(o);
            },
            r => unreachable!("unexpected rule for simple expression, `{:?}`", r)
        }
    }
    let rest = if let (Some(o), Some(r)) = (operator, rhs) {
        Some((o, r))
    } else {
        None
    };
    SimpleExpression { lhs: lhs.unwrap(), rest }
}

fn parse_complex_expression(
        pairs: Pairs<Rule>,
        handlebars: &Arc<Handlebars>,
        providers: &mut BTreeSet<String>,
        static_providers: &BTreeMap<String, json::Value>
    ) -> ComplexExpression
{
    let mut ret = ComplexExpression {
        combiner: Combiner::And,
        pieces: Vec::new(),
    };
    let mut append_to_previous = false;
    for pair in pairs {
        let rule = pair.as_rule();
        match rule {
            Rule::simple_expression | Rule::group_expression => {
                let new = match rule {
                    Rule::simple_expression =>
                        Expression::Simple(parse_simple_expression(pair, handlebars, providers, static_providers)),
                    Rule::group_expression =>
                        Expression::Complex(parse_complex_expression(pair.into_inner(), handlebars, providers, static_providers)),
                    _ => unreachable!("impossible"),
                };
                if append_to_previous {
                    // when we're in an "||" and we need to append an "&&"
                    append_to_previous = false;
                    if let Some(c) = {
                        match ret.pieces.last_mut().unwrap() {
                            Expression::Complex(c) => {
                                if let Combiner::And = c.combiner {
                                    Some(c)
                                } else {
                                    None
                                }
                            },
                            _ => None
                        }
                    } {
                        c.pieces.push(new);
                    } else {
                        let previous = ret.pieces.pop().unwrap();
                        let ce = ComplexExpression {
                            combiner: Combiner::And,
                            pieces: vec!(previous, new)
                        };
                        ret.pieces.push(Expression::Complex(ce));
                    }
                } else {
                    ret.pieces.push(new);
                }
            },
            Rule::combiner => {
                let c = match pair.as_str() {
                    "&&" => Combiner::And,
                    "||" => Combiner::Or,
                    c => unreachable!("unexpected combiner, `{:?}`", c),
                };
                if c != ret.combiner {
                    if ret.pieces.len() < 2 {
                        ret.combiner = c;
                    } else if c == Combiner::And {
                        append_to_previous = true;
                    } else {
                        ret = ComplexExpression {
                            combiner: c,
                            pieces: vec!(Expression::Complex(ret)),
                        }
                    }
                }
            },
            Rule::EOI => (),
            r => unreachable!("unexpected rule for complex expression, `{:?}`", r)
        }
    }
    ret
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json as json;
    use crate::template::join_helper;

    fn check_results(select: json::Value, data: json::Value, expect: &[json::Value], i: usize) {
        let select = create_select(select);
        let result: Vec<_> = select.as_iter(data).collect();
        assert_eq!(result.as_slice(), expect, "index {}", i)
    }

    fn create_select(json: json::Value) -> Select {
        let mut handlebars = Handlebars::new();
        handlebars.register_helper("join", Box::new(join_helper));
        handlebars.set_strict_mode(true);
        let eppp = json::from_value(json).unwrap();
        Select::new(eppp, &handlebars.into(), &Default::default())
    }

    #[test]
    fn get_providers() {
        // (select json, where clause, expected providers returned from `get_providers`, expected providers in `get_special_providers`)
        let check_table = vec!(
            (json::json!(4), None, vec!(), 0),
            (json::json!("c[0].d"), None, vec!("c"), 0),
            (json::json!("request.body[0].d"), None, vec!(), REQUEST_BODY),
            (json::json!(r#"request["start-line"]"#), None, vec!(), REQUEST_STARTLINE),
            (json::json!("repeat(5)"), None, vec!(), 0),
            (json::json!(r#"json_path("c.*.d")"#), None, vec!("c"), 0),
            (json::json!(r#"json_path("c.*.d")"#), Some("true && false && true || response.body.id == 123"), vec!("c"), RESPONSE_BODY),
            (json::json!(r#"json_path("c.foo.*.d")"#), None, vec!("c"), 0),
            (json::json!(r#"json_path("c.foo.*.d")"#), None, vec!("c"), 0),
            (json::json!(r#"json_path("response.headers.*.d")"#), None, vec!(), RESPONSE_HEADERS),
            (json::json!(r#"for_each[0]"#), None, vec!(), FOR_EACH),
            (json::json!(r#"stats.rtt"#), None, vec!(), STATS),
            (json::json!(r#"`{{join b.e "-"}}`"#), None, vec!("b"), 0),
            (
                json::json!({"z": 42, "dees": r#"json_path("c.*.d")"#, "x": "foo"}),
                None,
                vec!("c", "foo"),
                0
            )
        );

        for (i, (select, where_clause, providers_expect, rr_expect)) in check_table.into_iter().enumerate() {
            let s = if let Some(wc) = where_clause {
                create_select(json::json!({ "select": select, "where": wc }))
            } else {
                create_select(json::json!({ "select": select }))
            };
            let providers: Vec<_> = std::iter::FromIterator::from_iter(s.get_providers());
            let rr_providers = s.get_special_providers();
            assert_eq!(providers, providers_expect, "index {}", i);
            assert_eq!(rr_providers, rr_expect, "index {}", i);
        }

    }

    #[test]
    fn select() {
        let data = json::json!({
            "a": 3,
            "b": { "foo": "bar", "e": [5, 6, 7, 8] },
            "c": [
                { "d": 1 },
                { "d": 2 },
                { "d": 3 },
            ]
        });

        // (select json, expected out data)
        let check_table = vec!(
            (json::json!(4), vec!(json::json!(4))),
            (json::json!("c[0].d"), vec!(json::json!(1))),
            (json::json!(r#"json_path("c.*.d")"#), vec!(json::json!([1, 2, 3]))),
            (json::json!("repeat(5)"), vec!(json::json!([null, null, null, null, null]))),
            (json::json!("c.length"), vec!(json::json!(3))),
            (json::json!("b.e.length"), vec!(json::json!(4))),
            (json::json!(r#""foo-bar""#), vec!(json::json!("foo-bar"))),
            (json::json!("'foo-bar'"), vec!(json::json!("foo-bar"))),
            (json::json!(r#"`{{join b.e "-"}}`"#), vec!(json::json!("5-6-7-8"))),
            (
                json::json!({"z": 42, "dees": r#"json_path("c.*.d")"#}),
                vec!(json::json!({"z": 42, "dees": [1, 2, 3]}))
            )
        );

        for (i, (select, expect)) in check_table.into_iter().enumerate() {
            let data = data.clone();
            let s = json::json!({ "select": select });
            check_results(s, data, &expect, i);
        }

    }

    #[test]
    fn r#where() {
        let data = json::json!({
            "three": 3,
            "empty_object": {},
            "empty_array": [],
        });

        let three = vec!(json::json!(3));
        let empty = Vec::new();

        // (where clause, expected out data)
        let check_table = vec!(
            ("three > 2", &three),
            ("three > 3", &empty),
            ("three < 4", &three),
            ("three < 2", &empty),
            ("three != 2", &three),
            ("three != 3", &empty),
            ("three >= 3", &three),
            ("three >= 4", &empty),
            ("three <= 3", &three),
            ("three <= 2", &empty),
            ("three == 3", &three),
            ("three == 4", &empty),
            ("true", &three),
            ("false", &empty),
            ("1 > 2", &empty),
            (r#""""#, &empty),
            (r#""beep""#, &three),
            ("empty_object", &three),
            ("empty_array", &three),
            ("0", &empty),
            ("0.0", &empty),
            ("-3", &three),
            ("true && false", &empty),
            ("false || true", &three),
            ("true && false || true", &three),
            ("true && (false || true)", &three),
            ("(true && false) || true", &three),
            ("true && false || true", &three),
            ("false || true && false", &empty),
            ("false || (true || false) && false", &empty),
            ("false || (true && false) && true", &empty),
            ("false || (true || false) && true", &three),
        );

        for (i, (where_clause, expect)) in check_table.into_iter().enumerate() {
            let data = data.clone();
            let select = json::json!({
                "select": "three",
                "where": where_clause
            });
            check_results(select, data, expect, i);
        }
    }

    #[test]
    fn for_each() {
        let data = json::json!({
            "a": 3,
            "b": { "foo": "bar" },
            "c": [
                { "d": 1 },
                { "d": 2 },
                { "d": 3 },
            ]
        });

        // (select, for_each, expect)
        let check_table = vec!(
            (json::json!("a"), vec!("repeat(5)"), vec!(json::json!(3), json::json!(3), json::json!(3), json::json!(3), json::json!(3))),
            (json::json!("for_each[0]"), vec!(r#"json_path("c.*.d")"#), vec!(json::json!(1), json::json!(2), json::json!(3))),
            (json::json!("for_each[0]"), vec!("c"), vec!(json::json!({ "d": 1 }), json::json!({ "d": 2 }), json::json!({ "d": 3 }))),
            (
                json::json!("for_each[1]"),
                vec!("repeat(2)", r#"json_path("c.*.d")"#),
                vec!(json::json!(1), json::json!(2), json::json!(3), json::json!(1), json::json!(2), json::json!(3))
            ),
        );

        for (i, (select, for_each, expect)) in check_table.into_iter().enumerate() {
            let data = data.clone();
            let select = json::json!({
                "select": select,
                "for_each": for_each
            });
            check_results(select, data, &expect, i);
        }

    }
}