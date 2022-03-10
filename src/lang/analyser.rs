
use std::{collections::{HashSet, HashMap}, fmt::Display};

use super::{ast::{ExprData, Expr, DefKind, ExprType}, parser::Parser, error::{IBuilderResult, ErrorBuilder}, lexer::Span};

pub struct Analyser<'a> {
  src: &'a str,

  vars: HashMap<&'a str, ExprType>,

  // TODO: typecheck user defined types
  defined_types: HashSet<&'a str>
}

type AnalyserResult<'a> = IBuilderResult<ExprType>;

fn intrinsic_types<'a>() -> HashSet<&'a str> {
  HashSet::from([
    "uint8", "uint16", "uint32", "uint64",
    "int8",  "int16",  "int32",  "int64"
  ])
}

fn inconsistent_types<T: Display>(expected: T, got: T, name: &str, span: Span) -> ErrorBuilder {
  ErrorBuilder::new(
    &format!("Inconsistent type in definition of {n}:\n\tExpected {e}, got {g}",
      n=name,
      e=expected,
      g=got,
    ),
    span
  )
}

impl<'a> Analyser<'a> {
  pub fn analyse(src: &'a str) -> IBuilderResult<Vec<ExprData>> {
    let mut analyser = Self {
      src,
      vars: HashMap::new(),

      defined_types: intrinsic_types(),
    };

    let mut parser = Parser::new(src);
    let mut exprs = parser.parse_stmts()?;
    for expr in &mut exprs {
      analyser.analyse_stmt(expr)?;
    }

    Ok(exprs)
  }
  
  fn analyse_infix(
    &mut self,
    op:  &Span,
    lhs: &mut Box<ExprData>,
    rhs: &mut Box<ExprData>
  ) -> AnalyserResult {
    let lhs_type = self.analyse_expr(lhs)?;
    let rhs_type = self.analyse_expr(rhs)?;

    // TODO: proper type checking
    if lhs_type != rhs_type {
      return Err(ErrorBuilder::new(
        &format!(
          "Inconsistent types in infix expression, expected {} got {}.",
          lhs_type, rhs_type
        ),
        op.clone()
      ))
    }

    Ok(lhs_type)
  }

  fn analyse_fetch_var(&mut self, name: Span) -> AnalyserResult {
    let name_s = &self.src[name.clone()];
    match self.vars.get(name_s) {
      Some(t) => Ok(t.clone()),
      None => Err(
        ErrorBuilder::new(&format!("Undefined variable {}", name_s), name)
      )
    }
  }

  fn analyse_expr(&mut self, expr: &mut ExprData) -> AnalyserResult {
    match &mut expr.expr {
      Expr::NumberInt(_) => Ok(ExprType::UntypedU),
      Expr::Symbol => self.analyse_fetch_var(expr.span.clone()),
      Expr::Infix(op, lhs, rhs)
        => self.analyse_infix(op, lhs, rhs),
      _ => unimplemented!()
    }
  }

  fn analyse_defvar(
    &mut self,
    kind: &DefKind,
    name: &Span,
    ty:   &Option<Span>,
    expr: &mut Option<Box<ExprData>>
  ) -> AnalyserResult {
    match expr {
      // if there's a defined expression
      Some(expr) => {
        let expr_type = self.analyse_expr(expr)?;

        match ty {
          // infer type
          None => Ok(expr_type),

          // type already known
          Some(ty) => {
            // known type
            let known_ty = ExprType::from_str(&self.src[ty.clone()]);

            // if both types are user defined
            if let ExprType::UserDefined(known_ty_s) = known_ty.clone() {
              if let ExprType::UserDefined(expr_type_s) = expr_type.clone() {
                match known_ty_s == expr_type_s {
                  // types not equal
                  false =>
                    return Err(inconsistent_types(known_ty_s, expr_type_s, &self.src[name.clone()], name.clone())),

                  true => return Ok(known_ty)
                }
              }

              // a user defined type cannot transmute into an intrinsic
              return Err(inconsistent_types(known_ty_s, expr_type.to_string(), &self.src[name.clone()], name.clone()))
            }

            // check whether types are compatible
            // e.g def x i8 = 64; is possible because 64 can be transmuted to an i8 type
            let inferred = known_ty.transmute(expr_type.clone())
              .ok_or(
              inconsistent_types(known_ty, expr_type, &self.src[name.clone()], name.clone())
              )?;
            
            Ok(inferred)
          }
        }
      },

      None => Ok(ExprType::from_str(&self.src[ty.clone().unwrap()]))
    }
  }

  fn analyse_stmt(&mut self, stmt: &mut ExprData) -> AnalyserResult {
    match &mut stmt.expr {
      Expr::DefVariable { kind, name, ty, expr } => {
        let ty = self.analyse_defvar(kind, name, ty, expr)?;
        stmt.ty = Some(ty.clone());
        self.vars.insert(&self.src[name.clone()], ty.clone());
        Ok(ty)
      }
      _ => unimplemented!()
    }
  }
}