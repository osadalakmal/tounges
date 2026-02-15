use std::collections::BTreeMap;

use crate::dsl_error::DSLError;

#[derive(Debug, Clone, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(i64),
    String(String),
    Array(Vec<JsonValue>),
    Object(BTreeMap<String, JsonValue>),
}

impl JsonValue {
    pub fn to_json(&self) -> String {
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

    pub fn as_object(&self) -> Option<&BTreeMap<String, JsonValue>> {
        if let JsonValue::Object(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
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

pub struct JsonParser {
    chars: Vec<char>,
    i: usize,
}

impl JsonParser {
    pub fn new(src: &str) -> Self {
        Self {
            chars: src.chars().collect(),
            i: 0,
        }
    }

    pub fn parse(&mut self) -> Result<JsonValue, DSLError> {
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
