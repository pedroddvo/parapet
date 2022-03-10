use std::fmt::Display;

use super::lexer::Span;
#[derive(Debug)]

pub enum DefKind { Constant, Runtime }

#[derive(Debug)]
pub enum Expr {
  Symbol,
  NumberInt(u64),
  NumberFloat(f64),

  DefVariable {
    kind: DefKind,
    name: Span,
    ty: Option<Span>, // type
    expr: Option<Box<ExprData>>,
  },
  Infix(Span, Box<ExprData>, Box<ExprData>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprType {
  UntypedU, // not yet inferred type
  Uint8, Uint16, Uint32, Uint64,

  UntypedI, // not yet inferred type
  Int8, Int16, Int32, Int64,

  UserDefined(String),
}

#[derive(Debug)]
pub struct ExprData {
  pub expr: Expr,
  pub span: Span,
  pub ty: Option<ExprType>,
}

impl ExprData {
  pub fn untyped(span: Span, expr: Expr) -> Self {
    Self { expr, span, ty: None }
  }
}

impl ExprType {
  pub fn is_user_defined(&self) -> Option<&String> {
    use ExprType::*;
    match self {
      UserDefined(s) => Some(s),
      _ => None,
    }
  }

  pub fn is_signed(&self) -> bool {
    use ExprType::*;
    match self {
      Int8 | Int16 | Int32 | Int64 |
      UntypedI => true,

      _ => false,
    }
  }

  pub fn is_number(&self) -> bool {
    use ExprType::*;
    match self {
      Int8  | Int16  | Int32  | Int64  |
      Uint8 | Uint16 | Uint32 | Uint64 |
      UntypedI | UntypedU => true,

      _ => false
    }
  }

  // intrinsics
  pub fn from_str(str: &str) -> Self {
    use ExprType::*;
    match str {
      "uint8"  => Uint8, 
      "uint16" => Uint16, 
      "uint32" => Uint32, 
      "uint64" => Uint64, 

      "int8"  => Int8, 
      "int16" => Int16, 
      "int32" => Int32, 
      "int64" => Int64, 

      _ => UserDefined(String::from(str)),
    }
  }

  // from two types, attempt to transmute type
  // where b is the type to be transmuted
  // and self is the base type
  pub fn transmute<'a>(&self, b: Self) -> Option<Self> {
    use ExprType::*;

    if self.is_number() && b.is_number() {
      if !self.is_signed() && b.is_signed() { return None; }
      return Some(self.clone())
    }
    
    None
  }
}

impl Display for ExprType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use ExprType::*;
    let t = match self {
      n @
      (Int8  | Int16  | Int32  | Int64 |
       Uint8 | Uint16 | Uint32 | Uint64)
        => format!("{:?}", n).to_lowercase(),

      UserDefined(s) => s.clone(),

      UntypedI => String::from("untyped integer"),
      UntypedU => String::from("untyped number"),

      n => format!("{:?}", n)
    };

    write!(f, "{}", t)?;

    Ok(())
  }
}