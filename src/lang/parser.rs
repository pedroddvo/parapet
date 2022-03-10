use std::collections::HashMap;

use super::lexer::{Token, Span, lexer};
use super::error::{ErrorBuilder, IBuilderResult};
use super::ast::*;

pub type TokenSpan = (Token, Span);

pub struct Parser<'a> {
  src: &'a str,
  lexer: logos::Lexer<'a, Token>,

  tokens: Vec<TokenSpan>,

  infix_bps: HashMap<&'a str, (u8, u8)>,
}

impl<'a> Parser<'a> {
  pub fn new(src: &'a str) -> Self {
    Self {
      src,
      lexer: lexer(src),
      tokens: vec![],

      infix_bps: HashMap::from([
        ("+", (10, 11)),
        ("-", (10, 11)),
        ("*", (12, 13)),
        ("/", (12, 13)),
      ])
    }
  }

  fn next(&mut self) -> Option<TokenSpan> {
    if !self.tokens.is_empty() { return self.tokens.pop(); }
    let tok = self.lexer.next()?;
    let span = self.lexer.span();
    Some((tok, span))
  }

  fn peek(&mut self) -> Option<&TokenSpan> {
    if !self.tokens.is_empty() { return self.tokens.last() }
    let tok = self.lexer.next()?;
    let span = self.lexer.span();
    self.tokens.push((tok, span));
    self.tokens.last()
  }

  fn peek_eof(&mut self) -> IBuilderResult<&TokenSpan> {
    let len = self.src.len();
    self.peek().ok_or(ErrorBuilder::new("Unexpected end of file!", len..len))
  }

  fn next_eof(&mut self) -> IBuilderResult<TokenSpan> {
    self.next().ok_or(ErrorBuilder::new("Unexpected end of file!", self.src.len()..self.src.len()))
  }

  fn skip_tokens(&mut self, toks: &'static [Token]) {
    loop {
      let tok = match self.peek() {
        Some((t, ..)) => t,
        None => break,
      };

      if !toks.contains(tok) { break; }
      self.next();
    }
  }

  fn expect_token(&mut self, toks: &'static [Token], fail_msg: &'static str) -> IBuilderResult<TokenSpan> {
    let (tok, span) = self.peek_eof()?;
    match toks.contains(tok) {
      true  => Ok(self.next().unwrap()),
      false => Err(ErrorBuilder::new(fail_msg, span.clone())),
    }
  }

  fn accept_token(&mut self, toks: &'static [Token]) -> Option<TokenSpan> {
    let tok_r = self.peek_eof();
    match tok_r.is_ok() && toks.contains(&tok_r.unwrap().0) {
      true  => Some(self.next().unwrap()),
      false => None
    }
  }

  pub fn parse_atomic(&mut self) -> IBuilderResult<ExprData> {
    let (t_tok, t_span) = self.next_eof()?;

    match t_tok {
      Token::NumberInt => Ok(ExprData::untyped(
        t_span.clone(),
        Expr::NumberInt(self.src[t_span].parse::<u64>().unwrap()),
      )),
      Token::NumberHex => Ok(ExprData::untyped(
        t_span.clone(),
        // [2..] to trim the '0x'
        Expr::NumberInt(u64::from_str_radix(&self.src[t_span][2..], 16).unwrap()),
      )),
      Token::NumberFloat => Ok(ExprData::untyped(
        t_span.clone(),
        Expr::NumberFloat(self.src[t_span].parse::<f64>().unwrap()),
      )),

      Token::Symbol => Ok(ExprData::untyped(t_span.clone(), Expr::Symbol)),

      Token::LParen => {
        let lhs = self.parse_expr()?;
        self.expect_token(&[Token::RParen], "Expected a closing parenthesis!")?;
        Ok(lhs)
      }

      _ => panic!("parse_atomic bad token: {:?}", (t_tok, t_span)),
    }
  }

  pub fn parse_binary(&mut self, min_bp: u8) -> IBuilderResult<ExprData> {
    let mut lhs = self.parse_atomic()?;

    loop {
      let (op_t, op_s) = match self.peek() {
        Some((op_t, op_s)) => (op_t, op_s.clone()),
        None => break,
      };

      match op_t {
        Token::Operator => {},
        _ => break,
      }

      if let Some(&(l_bp, r_bp)) = self.infix_bps.get(&self.src[op_s.clone()]) {
        if l_bp < min_bp {
          break;
        }

        self.next_eof()?;
        self.skip_tokens(&[Token::Newline]);
        let rhs = self.parse_binary(r_bp)?;

        lhs = ExprData::untyped(
          lhs.span.start..rhs.span.end,
          Expr::Infix(op_s, Box::new(lhs), Box::new(rhs)),
        );
        continue;
      }

      break;
    }

    Ok(lhs)
  }
  
  pub fn parse_expr(&mut self) -> IBuilderResult<ExprData> {
    self.parse_binary(0)
  }

  // parse 'def' statements
  pub fn parse_def(&mut self) -> IBuilderResult<ExprData> {
    // def keyword token
    let (.., def_s) = self.next_eof()?;
    self.skip_tokens(&[Token::Newline]);
    // name of the variable
    let (.., name_s) =
      self.expect_token(&[Token::Symbol], "Expected a symbol after 'def' keyword!")?;

    match self.next_eof()? {
      // variable definition with non-inferred type
      (Token::Symbol, ty_s) => {
        // if next token is newline, it's an undefined declaration
        // the declaration will have an undefied value
        if let Some(_) = self.accept_token(&[Token::Newline]) {
          return Ok(ExprData::untyped(
            def_s.start..ty_s.end,
            Expr::DefVariable {
              kind: DefKind::Runtime,
              name: name_s,
              ty: Some(ty_s),
              expr: None,
            }
          ))
        }

        // else, expect an equals and expression
        self.expect_token(&[Token::Equals], "Expected an equals after variable definition!")?;
        self.skip_tokens(&[Token::Newline]); // allow newlines after =
        let expr = self.parse_expr()?;

        Ok(ExprData::untyped(
          def_s.start..expr.span.end,
          Expr::DefVariable {
            kind: DefKind::Runtime,
            name: name_s,
            ty: Some(ty_s),
            expr: Some(Box::new(expr)),
          }
        ))
      },
      // variable definition with inferred type
      (Token::Equals, ..) => {
        self.skip_tokens(&[Token::Newline]); // allow newlines after =
        let expr = self.parse_expr()?;

        Ok(ExprData::untyped(
          def_s.start..expr.span.end,
          Expr::DefVariable {
            kind: DefKind::Runtime,
            name: name_s,
            ty: None,
            expr: Some(Box::new(expr)),
          }
        ))
      }
      _ => unreachable!()
    }
  }

  pub fn parse_stmt(&mut self) -> IBuilderResult<ExprData> {
    let (tok_t, tok_s) = self.peek_eof()?;

    match tok_t {
      Token::KeywordDef => self.parse_def(),
      _ => self.parse_expr()
    }
  }

  pub fn parse_stmts(&mut self) -> IBuilderResult<Vec<ExprData>> {
    let mut stmts = vec![];
    loop {
      stmts.push(self.parse_stmt()?);
      if self.peek().is_none() { break; }
      self.expect_token(&[Token::Newline], "Expected a statement!")?;
    }

    Ok(stmts)
  }
}