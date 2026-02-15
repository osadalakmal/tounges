use crate::ast::{Expr, Stmt};
use crate::dsl_error::DSLError;
use crate::json::JsonValue;
use crate::lexer::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    i: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, i: 0 }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Stmt>, DSLError> {
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
        value.is_none_or(|v| self.peek().value == v)
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
