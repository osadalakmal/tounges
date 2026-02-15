use crate::dsl_error::DSLError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Number,
    Ident,
    Keyword,
    Symbol,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

const KEYWORDS: &[&str] = &[
    "if", "else", "while", "not", "and", "or", "true", "false", "output",
];

pub struct Lexer {
    chars: Vec<char>,
    i: usize,
}

impl Lexer {
    pub fn new(src: &str) -> Self {
        Self {
            chars: src.chars().collect(),
            i: 0,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, DSLError> {
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
