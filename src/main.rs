use std::collections::BTreeMap;
use std::io::{self, Read};

#[derive(Debug)]
struct DSLError(String);

impl std::fmt::Display for DSLError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for DSLError {}

#[derive(Debug, Clone, PartialEq)]
enum JsonValue {
    Null,
    Bool(bool),
    Number(i64),
    String(String),
    Array(Vec<JsonValue>),
    Object(BTreeMap<String, JsonValue>),
}

impl JsonValue {
    fn to_json(&self) -> String {
        match self {
            JsonValue::Null => "null".to_string(),
            JsonValue::Bool(b) => b.to_string(),
            JsonValue::Number(n) => n.to_string(),
            JsonValue::String(s) => format!("\"{}\"", escape_json(s)),
            JsonValue::Array(items) => {
                let parts: Vec<String> = items.iter().map(|v| v.to_json()).collect();
                format!("[{}]", parts.join(","))
            }
            JsonValue::Object(map) => {
                let parts: Vec<String> = map
                    .iter()
                    .map(|(k, v)| format!("\"{}\":{}", escape_json(k), v.to_json()))
                    .collect();
                format!("{{{}}}", parts.join(","))
            }
        }
    }

    fn as_object(&self) -> Option<&BTreeMap<String, JsonValue>> {
        if let JsonValue::Object(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    fn as_i64(&self) -> Option<i64> {
        if let JsonValue::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }
}

fn escape_json(input: &str) -> String {
    input
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

struct JsonParser {
    chars: Vec<char>,
    i: usize,
}

impl JsonParser {
    fn new(src: &str) -> Self {
        Self {
            chars: src.chars().collect(),
            i: 0,
        }
    }

    fn parse(&mut self) -> Result<JsonValue, DSLError> {
        self.skip_ws();
        let v = self.parse_value()?;
        self.skip_ws();
        if self.i != self.chars.len() {
            return Err(DSLError("Trailing characters in JSON".to_string()));
        }
        Ok(v)
    }

    fn parse_value(&mut self) -> Result<JsonValue, DSLError> {
        self.skip_ws();
        if self.i >= self.chars.len() {
            return Err(DSLError("Unexpected EOF in JSON".to_string()));
        }
        match self.chars[self.i] {
            '{' => self.parse_object(),
            '[' => self.parse_array(),
            '"' => Ok(JsonValue::String(self.parse_string()?)),
            't' => {
                self.expect_word("true")?;
                Ok(JsonValue::Bool(true))
            }
            'f' => {
                self.expect_word("false")?;
                Ok(JsonValue::Bool(false))
            }
            'n' => {
                self.expect_word("null")?;
                Ok(JsonValue::Null)
            }
            '-' | '0'..='9' => Ok(JsonValue::Number(self.parse_number()?)),
            c => Err(DSLError(format!("Unexpected JSON character: {c}"))),
        }
    }

    fn parse_object(&mut self) -> Result<JsonValue, DSLError> {
        self.expect_char('{')?;
        self.skip_ws();
        let mut map = BTreeMap::new();
        if self.peek_char() == Some('}') {
            self.i += 1;
            return Ok(JsonValue::Object(map));
        }
        loop {
            self.skip_ws();
            let key = self.parse_string()?;
            self.skip_ws();
            self.expect_char(':')?;
            let val = self.parse_value()?;
            map.insert(key, val);
            self.skip_ws();
            match self.peek_char() {
                Some(',') => self.i += 1,
                Some('}') => {
                    self.i += 1;
                    break;
                }
                _ => return Err(DSLError("Expected ',' or '}' in object".to_string())),
            }
        }
        Ok(JsonValue::Object(map))
    }

    fn parse_array(&mut self) -> Result<JsonValue, DSLError> {
        self.expect_char('[')?;
        self.skip_ws();
        let mut out = Vec::new();
        if self.peek_char() == Some(']') {
            self.i += 1;
            return Ok(JsonValue::Array(out));
        }
        loop {
            out.push(self.parse_value()?);
            self.skip_ws();
            match self.peek_char() {
                Some(',') => self.i += 1,
                Some(']') => {
                    self.i += 1;
                    break;
                }
                _ => return Err(DSLError("Expected ',' or ']' in array".to_string())),
            }
        }
        Ok(JsonValue::Array(out))
    }

    fn parse_string(&mut self) -> Result<String, DSLError> {
        self.expect_char('"')?;
        let mut out = String::new();
        while self.i < self.chars.len() {
            let c = self.chars[self.i];
            self.i += 1;
            if c == '"' {
                return Ok(out);
            }
            if c == '\\' {
                if self.i >= self.chars.len() {
                    return Err(DSLError("Invalid escape in JSON string".to_string()));
                }
                let esc = self.chars[self.i];
                self.i += 1;
                let translated = match esc {
                    '"' => '"',
                    '\\' => '\\',
                    '/' => '/',
                    'b' => '\x08',
                    'f' => '\x0c',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    _ => return Err(DSLError("Unsupported JSON escape".to_string())),
                };
                out.push(translated);
            } else {
                out.push(c);
            }
        }
        Err(DSLError("Unterminated JSON string".to_string()))
    }

    fn parse_number(&mut self) -> Result<i64, DSLError> {
        let start = self.i;
        if self.peek_char() == Some('-') {
            self.i += 1;
        }
        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() {
                self.i += 1;
            } else {
                break;
            }
        }
        let s: String = self.chars[start..self.i].iter().collect();
        s.parse::<i64>()
            .map_err(|_| DSLError(format!("Invalid JSON number: {s}")))
    }

    fn expect_word(&mut self, w: &str) -> Result<(), DSLError> {
        for ch in w.chars() {
            self.expect_char(ch)?;
        }
        Ok(())
    }

    fn expect_char(&mut self, c: char) -> Result<(), DSLError> {
        if self.peek_char() == Some(c) {
            self.i += 1;
            Ok(())
        } else {
            Err(DSLError(format!("Expected character {c}")))
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.get(self.i).copied()
    }

    fn skip_ws(&mut self) {
        while self.i < self.chars.len() && self.chars[self.i].is_whitespace() {
            self.i += 1;
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokenKind {
    Number,
    Ident,
    Keyword,
    Symbol,
    Eof,
}

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    value: String,
}

const KEYWORDS: &[&str] = &[
    "if", "else", "while", "not", "and", "or", "true", "false", "output",
];

struct Lexer {
    chars: Vec<char>,
    i: usize,
}

impl Lexer {
    fn new(src: &str) -> Self {
        Self {
            chars: src.chars().collect(),
            i: 0,
        }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, DSLError> {
        let mut tokens = Vec::new();
        while self.i < self.chars.len() {
            let ch = self.chars[self.i];
            if ch.is_whitespace() {
                self.i += 1;
            } else if ch == '#' {
                while self.i < self.chars.len() && self.chars[self.i] != '\n' {
                    self.i += 1;
                }
            } else if ch.is_ascii_digit() {
                tokens.push(self.number());
            } else if ch.is_ascii_alphabetic() || ch == '_' {
                tokens.push(self.identifier());
            } else if self.peek2_is("==")
                || self.peek2_is("!=")
                || self.peek2_is("<=")
                || self.peek2_is(">=")
            {
                let v = format!("{}{}", self.chars[self.i], self.chars[self.i + 1]);
                tokens.push(Token {
                    kind: TokenKind::Symbol,
                    value: v,
                });
                self.i += 2;
            } else if "<>=".contains(ch) || "+-*/(){};".contains(ch) {
                tokens.push(Token {
                    kind: TokenKind::Symbol,
                    value: ch.to_string(),
                });
                self.i += 1;
            } else {
                return Err(DSLError(format!("Unexpected character: {ch}")));
            }
        }
        tokens.push(Token {
            kind: TokenKind::Eof,
            value: String::new(),
        });
        Ok(tokens)
    }

    fn peek2_is(&self, s: &str) -> bool {
        if self.i + 1 >= self.chars.len() {
            return false;
        }
        format!("{}{}", self.chars[self.i], self.chars[self.i + 1]) == s
    }

    fn number(&mut self) -> Token {
        let start = self.i;
        while self.i < self.chars.len() && self.chars[self.i].is_ascii_digit() {
            self.i += 1;
        }
        Token {
            kind: TokenKind::Number,
            value: self.chars[start..self.i].iter().collect(),
        }
    }

    fn identifier(&mut self) -> Token {
        let start = self.i;
        while self.i < self.chars.len()
            && (self.chars[self.i].is_ascii_alphanumeric() || self.chars[self.i] == '_')
        {
            self.i += 1;
        }
        let value: String = self.chars[start..self.i].iter().collect();
        let kind = if KEYWORDS.contains(&value.as_str()) {
            TokenKind::Keyword
        } else {
            TokenKind::Ident
        };
        Token { kind, value }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Literal(JsonValue),
    Var(String),
    Unary {
        op: String,
        expr: Box<Expr>,
    },
    Binary {
        op: String,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
enum Stmt {
    Assign {
        name: String,
        expr: Expr,
    },
    Output {
        expr: Expr,
    },
    If {
        cond: Expr,
        then_block: Vec<Stmt>,
        else_block: Vec<Stmt>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
}

struct Parser {
    tokens: Vec<Token>,
    i: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, i: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Stmt>, DSLError> {
        let mut out = Vec::new();
        while !self.check(TokenKind::Eof, None) {
            if self.match_symbol(";") {
                continue;
            }
            out.push(self.parse_statement()?);
            self.match_symbol(";");
        }
        Ok(out)
    }

    fn parse_statement(&mut self) -> Result<Stmt, DSLError> {
        if self.match_keyword("if") {
            let cond = self.parse_expression()?;
            let then_block = self.parse_block()?;
            let else_block = if self.match_keyword("else") {
                self.parse_block()?
            } else {
                Vec::new()
            };
            return Ok(Stmt::If {
                cond,
                then_block,
                else_block,
            });
        }
        if self.match_keyword("while") {
            let cond = self.parse_expression()?;
            let body = self.parse_block()?;
            return Ok(Stmt::While { cond, body });
        }
        if self.match_keyword("output") {
            let expr = self.parse_expression()?;
            return Ok(Stmt::Output { expr });
        }
        if self.check(TokenKind::Ident, None) && self.check_next_symbol("=") {
            let name = self.advance().value;
            self.expect_symbol("=")?;
            let expr = self.parse_expression()?;
            return Ok(Stmt::Assign { name, expr });
        }
        Err(DSLError("Unexpected token in statement".to_string()))
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, DSLError> {
        self.expect_symbol("{")?;
        let mut out = Vec::new();
        while !self.match_symbol("}") {
            if self.check(TokenKind::Eof, None) {
                return Err(DSLError("Unterminated block".to_string()));
            }
            if self.match_symbol(";") {
                continue;
            }
            out.push(self.parse_statement()?);
            self.match_symbol(";");
        }
        Ok(out)
    }

    fn parse_expression(&mut self) -> Result<Expr, DSLError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, DSLError> {
        let mut expr = self.parse_and()?;
        while self.match_keyword("or") {
            expr = Expr::Binary {
                op: "or".to_string(),
                left: Box::new(expr),
                right: Box::new(self.parse_and()?),
            };
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, DSLError> {
        let mut expr = self.parse_equality()?;
        while self.match_keyword("and") {
            expr = Expr::Binary {
                op: "and".to_string(),
                left: Box::new(expr),
                right: Box::new(self.parse_equality()?),
            };
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, DSLError> {
        let mut expr = self.parse_comparison()?;
        loop {
            if self.match_symbol("==") {
                expr = Expr::Binary {
                    op: "==".to_string(),
                    left: Box::new(expr),
                    right: Box::new(self.parse_comparison()?),
                };
            } else if self.match_symbol("!=") {
                expr = Expr::Binary {
                    op: "!=".to_string(),
                    left: Box::new(expr),
                    right: Box::new(self.parse_comparison()?),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, DSLError> {
        let mut expr = self.parse_term()?;
        loop {
            let op = if self.match_symbol("<") {
                Some("<")
            } else if self.match_symbol("<=") {
                Some("<=")
            } else if self.match_symbol(">") {
                Some(">")
            } else if self.match_symbol(">=") {
                Some(">=")
            } else {
                None
            };
            if let Some(op) = op {
                expr = Expr::Binary {
                    op: op.to_string(),
                    left: Box::new(expr),
                    right: Box::new(self.parse_term()?),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, DSLError> {
        let mut expr = self.parse_factor()?;
        loop {
            let op = if self.match_symbol("+") {
                Some("+")
            } else if self.match_symbol("-") {
                Some("-")
            } else {
                None
            };
            if let Some(op) = op {
                expr = Expr::Binary {
                    op: op.to_string(),
                    left: Box::new(expr),
                    right: Box::new(self.parse_factor()?),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, DSLError> {
        let mut expr = self.parse_unary()?;
        loop {
            let op = if self.match_symbol("*") {
                Some("*")
            } else if self.match_symbol("/") {
                Some("/")
            } else {
                None
            };
            if let Some(op) = op {
                expr = Expr::Binary {
                    op: op.to_string(),
                    left: Box::new(expr),
                    right: Box::new(self.parse_unary()?),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, DSLError> {
        if self.match_symbol("-") {
            return Ok(Expr::Unary {
                op: "-".to_string(),
                expr: Box::new(self.parse_unary()?),
            });
        }
        if self.match_keyword("not") {
            return Ok(Expr::Unary {
                op: "not".to_string(),
                expr: Box::new(self.parse_unary()?),
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, DSLError> {
        if self.match_kind(TokenKind::Number) {
            let n = self
                .previous()
                .value
                .parse::<i64>()
                .map_err(|_| DSLError("Invalid integer literal".to_string()))?;
            return Ok(Expr::Literal(JsonValue::Number(n)));
        }
        if self.match_keyword("true") {
            return Ok(Expr::Literal(JsonValue::Bool(true)));
        }
        if self.match_keyword("false") {
            return Ok(Expr::Literal(JsonValue::Bool(false)));
        }
        if self.match_kind(TokenKind::Ident) {
            return Ok(Expr::Var(self.previous().value.clone()));
        }
        if self.match_symbol("(") {
            let expr = self.parse_expression()?;
            self.expect_symbol(")")?;
            return Ok(expr);
        }
        Err(DSLError("Unexpected token in expression".to_string()))
    }

    fn check(&self, kind: TokenKind, value: Option<&str>) -> bool {
        if self.peek().kind != kind {
            return false;
        }
        value.map_or(true, |v| self.peek().value == v)
    }

    fn check_next_symbol(&self, symbol: &str) -> bool {
        if self.i + 1 >= self.tokens.len() {
            return false;
        }
        self.tokens[self.i + 1].kind == TokenKind::Symbol && self.tokens[self.i + 1].value == symbol
    }

    fn match_kind(&mut self, kind: TokenKind) -> bool {
        if self.check(kind, None) {
            self.i += 1;
            true
        } else {
            false
        }
    }

    fn match_keyword(&mut self, keyword: &str) -> bool {
        if self.check(TokenKind::Keyword, Some(keyword)) {
            self.i += 1;
            true
        } else {
            false
        }
    }

    fn match_symbol(&mut self, symbol: &str) -> bool {
        if self.check(TokenKind::Symbol, Some(symbol)) {
            self.i += 1;
            true
        } else {
            false
        }
    }

    fn expect_symbol(&mut self, symbol: &str) -> Result<(), DSLError> {
        if self.match_symbol(symbol) {
            Ok(())
        } else {
            Err(DSLError(format!("Expected symbol {symbol}")))
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.i]
    }
    fn previous(&self) -> &Token {
        &self.tokens[self.i - 1]
    }
    fn advance(&mut self) -> Token {
        let t = self.tokens[self.i].clone();
        self.i += 1;
        t
    }
}

struct Interpreter {
    vars: BTreeMap<String, JsonValue>,
    outputs: Vec<JsonValue>,
}

impl Interpreter {
    fn new(vars: BTreeMap<String, JsonValue>) -> Self {
        Self {
            vars,
            outputs: Vec::new(),
        }
    }

    fn run(&mut self, program: &[Stmt]) -> Result<JsonValue, DSLError> {
        for stmt in program {
            self.exec_stmt(stmt)?;
        }
        let mut out = BTreeMap::new();
        out.insert(
            "variables".to_string(),
            JsonValue::Object(self.vars.clone()),
        );
        out.insert(
            "outputs".to_string(),
            JsonValue::Array(self.outputs.clone()),
        );
        Ok(JsonValue::Object(out))
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Result<(), DSLError> {
        match stmt {
            Stmt::Assign { name, expr } => {
                let v = self.eval_expr(expr)?;
                self.vars.insert(name.clone(), v);
            }
            Stmt::Output { expr } => {
                let v = self.eval_expr(expr)?;
                self.outputs.push(v);
            }
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                let branch = if truthy(&self.eval_expr(cond)?) {
                    then_block
                } else {
                    else_block
                };
                for s in branch {
                    self.exec_stmt(s)?;
                }
            }
            Stmt::While { cond, body } => {
                while truthy(&self.eval_expr(cond)?) {
                    for s in body {
                        self.exec_stmt(s)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<JsonValue, DSLError> {
        match expr {
            Expr::Literal(v) => Ok(v.clone()),
            Expr::Var(name) => self
                .vars
                .get(name)
                .cloned()
                .ok_or_else(|| DSLError(format!("Undefined variable: {name}"))),
            Expr::Unary { op, expr } => {
                let v = self.eval_expr(expr)?;
                match op.as_str() {
                    "-" => Ok(JsonValue::Number(-as_i64(&v)?)),
                    "not" => Ok(JsonValue::Bool(!truthy(&v))),
                    _ => Err(DSLError("Unknown unary operator".to_string())),
                }
            }
            Expr::Binary { op, left, right } => {
                let l = self.eval_expr(left)?;
                if op == "and" {
                    return Ok(JsonValue::Bool(
                        truthy(&l) && truthy(&self.eval_expr(right)?),
                    ));
                }
                if op == "or" {
                    return Ok(JsonValue::Bool(
                        truthy(&l) || truthy(&self.eval_expr(right)?),
                    ));
                }
                let r = self.eval_expr(right)?;
                apply_binary(op, &l, &r)
            }
        }
    }
}

fn truthy(v: &JsonValue) -> bool {
    match v {
        JsonValue::Bool(b) => *b,
        JsonValue::Number(n) => *n != 0,
        JsonValue::Null => false,
        JsonValue::String(s) => !s.is_empty(),
        JsonValue::Array(a) => !a.is_empty(),
        JsonValue::Object(o) => !o.is_empty(),
    }
}

fn as_i64(v: &JsonValue) -> Result<i64, DSLError> {
    v.as_i64()
        .ok_or_else(|| DSLError("Expected integer".to_string()))
}

fn apply_binary(op: &str, l: &JsonValue, r: &JsonValue) -> Result<JsonValue, DSLError> {
    Ok(match op {
        "+" => JsonValue::Number(as_i64(l)? + as_i64(r)?),
        "-" => JsonValue::Number(as_i64(l)? - as_i64(r)?),
        "*" => JsonValue::Number(as_i64(l)? * as_i64(r)?),
        "/" => JsonValue::Number(as_i64(l)? / as_i64(r)?),
        "==" => JsonValue::Bool(l == r),
        "!=" => JsonValue::Bool(l != r),
        "<" => JsonValue::Bool(as_i64(l)? < as_i64(r)?),
        "<=" => JsonValue::Bool(as_i64(l)? <= as_i64(r)?),
        ">" => JsonValue::Bool(as_i64(l)? > as_i64(r)?),
        ">=" => JsonValue::Bool(as_i64(l)? >= as_i64(r)?),
        _ => return Err(DSLError("Unknown binary operator".to_string())),
    })
}

fn run_program(source: &str, input: BTreeMap<String, JsonValue>) -> Result<JsonValue, DSLError> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()?;
    let mut interpreter = Interpreter::new(input);
    interpreter.run(&program)
}

fn parse_payload(input: &str) -> Result<(String, BTreeMap<String, JsonValue>), DSLError> {
    let root = JsonParser::new(input).parse()?;
    let obj = root
        .as_object()
        .ok_or_else(|| DSLError("Input payload must be a JSON object".to_string()))?;
    let program = match obj.get("program") {
        Some(JsonValue::String(s)) => s.clone(),
        _ => return Err(DSLError("`program` must be a string".to_string())),
    };
    let vars = match obj.get("input") {
        None => BTreeMap::new(),
        Some(JsonValue::Object(map)) => map.clone(),
        Some(_) => return Err(DSLError("`input` must be a JSON object".to_string())),
    };
    Ok((program, vars))
}

fn error_json(message: &str) -> String {
    let mut obj = BTreeMap::new();
    obj.insert("error".to_string(), JsonValue::String(message.to_string()));
    JsonValue::Object(obj).to_json()
}

fn main() {
    let mut buf = String::new();
    let exit = match io::stdin().read_to_string(&mut buf) {
        Ok(_) => match parse_payload(&buf) {
            Ok((program, vars)) => match run_program(&program, vars) {
                Ok(out) => {
                    println!("{}", out.to_json());
                    0
                }
                Err(e) => {
                    println!("{}", error_json(&e.to_string()));
                    1
                }
            },
            Err(e) => {
                println!("{}", error_json(&e.to_string()));
                1
            }
        },
        Err(e) => {
            println!("{}", error_json(&format!("Failed reading stdin: {e}")));
            1
        }
    };
    std::process::exit(exit);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arithmetic_and_output() {
        let result = run_program("x = 3 + 4 * 2; output x;", BTreeMap::new()).unwrap();
        let obj = result.as_object().unwrap();
        assert_eq!(
            obj.get("outputs"),
            Some(&JsonValue::Array(vec![JsonValue::Number(11)]))
        );
    }

    #[test]
    fn if_else_and_boolean_operators() {
        let program = "flag = true and not false; if flag { output 1; } else { output 0; }";
        let result = run_program(program, BTreeMap::new()).unwrap();
        let obj = result.as_object().unwrap();
        assert_eq!(
            obj.get("outputs"),
            Some(&JsonValue::Array(vec![JsonValue::Number(1)]))
        );
    }

    #[test]
    fn while_loop() {
        let program = "acc = 0; i = 0; while i < 4 { acc = acc + i; i = i + 1; } output acc;";
        let result = run_program(program, BTreeMap::new()).unwrap();
        let obj = result.as_object().unwrap();
        assert_eq!(
            obj.get("outputs"),
            Some(&JsonValue::Array(vec![JsonValue::Number(6)]))
        );
    }

    #[test]
    fn supports_json_input_vars() {
        let mut input = BTreeMap::new();
        input.insert("base".to_string(), JsonValue::Number(7));
        let result = run_program("total = base + 5; output total;", input).unwrap();
        let obj = result.as_object().unwrap();
        assert_eq!(
            obj.get("outputs"),
            Some(&JsonValue::Array(vec![JsonValue::Number(12)]))
        );
    }
}
