use super::lexer::{Span};

#[derive(Debug)]
pub struct ErrorContext {
    span: Span,
    msg: String
}

#[derive(Debug)]
pub struct ErrorBuilder {
    root: ErrorContext,
    context: Vec<ErrorContext>,
}

pub type IBuilderResult<T> = Result<T, ErrorBuilder>;

impl ErrorBuilder {
    pub fn new(msg: &str, span: Span) -> Self {
        Self {
            root: ErrorContext { msg: String::from(msg), span },
            context: vec![],
        }
    }

    pub fn context(&mut self, msg: &str, span: Span) -> &mut Self {
        self.context.push(ErrorContext { msg: String::from(msg), span });
        self
    }
}
