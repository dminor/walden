use crate::lexer;
use std::collections::LinkedList;
use std::error::Error;
use std::fmt;

/*
keyword        -> binary | binary (IDENTIFIER ":" binary)+
binary         -> unary | unary ("+"|"-"|"*"|"/"|">"|">="|"<"|"<="|"~="|"=" unary)*
unary          -> value | unary IDENTIFIER
value          -> IDENTIFIER | NUMBER | STRING | "false" | "true" | "nil"
*/

pub enum Ast {
    Unary(Box<Ast>, Box<Ast>),
    Value(lexer::LexedToken),
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ast::Unary(obj, msg) => write!(f, "(unary {} {})", *obj, *msg),
            Ast::Value(t) => match &t.token {
                lexer::Token::Identifier(_) => write!(f, "{}:Identifier", t.token),
                lexer::Token::Number(_) => write!(f, "{}:Number", t.token),
                lexer::Token::String(_) => write!(f, "{}:String", t.token),
                _ => write!(f, "{}", t.token),
            },
        }
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub err: String,
    pub line: usize,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParserError: {}", self.err)
    }
}

impl Error for ParserError {}

fn unary(tokens: &mut LinkedList<lexer::LexedToken>) -> Result<Ast, ParserError> {
    let v = value(tokens);

    match v {
        Ok(ast) => {
            let mut result;
            result = ast;
            loop {
                match tokens.front() {
                    Some(peek) => match peek.token {
                        lexer::Token::Identifier(_) => match value(tokens) {
                            Ok(msg) => {
                                result = Ast::Unary(Box::new(result), Box::new(msg));
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        },
                        _ => {
                            break;
                        }
                    },
                    _ => {
                        break;
                    }
                }
            }
            Ok(result)
        }
        Err(e) => {
            return Err(e);
        }
    }
}

fn value(tokens: &mut LinkedList<lexer::LexedToken>) -> Result<Ast, ParserError> {
    match tokens.pop_front() {
        Some(token) => match token.token {
            lexer::Token::False => Ok(Ast::Value(token)),
            lexer::Token::Identifier(_) => Ok(Ast::Value(token)),
            lexer::Token::Number(_) => Ok(Ast::Value(token)),
            lexer::Token::Nil => Ok(Ast::Value(token)),
            lexer::Token::String(_) => Ok(Ast::Value(token)),
            lexer::Token::True => Ok(Ast::Value(token)),
            lexer::Token::Minus => match tokens.front() {
                Some(peek) => match peek.token {
                    lexer::Token::Number(n) => {
                        let result = Ok(Ast::Value(lexer::LexedToken {
                            token: lexer::Token::Number(-1.0 * n),
                            line: token.line,
                        }));
                        tokens.pop_front();
                        return result;
                    }
                    _ => Err(ParserError {
                        err: "Expected value, found -.".to_string(),
                        line: token.line,
                    }),
                },
                _ => Err(ParserError {
                    err: "Expected value, found -.".to_string(),
                    line: token.line,
                }),
            },
            _ => {
                let mut err = "Expected value, found ".to_string();
                err.push_str(&token.token.to_string());
                err.push('.');
                Err(ParserError {
                    err: err,
                    line: token.line,
                })
            }
        },
        None => {
            return Err(ParserError {
                err: "Unexpected end of input.".to_string(),
                line: usize::max_value(),
            });
        }
    }
}

pub fn parse(tokens: &mut LinkedList<lexer::LexedToken>) -> Result<Ast, ParserError> {
    unary(tokens)
}

#[cfg(test)]
mod tests {
    use crate::lexer;
    use crate::parser;

    macro_rules! parse {
        ($input:expr, $value:expr) => {{
            match lexer::scan($input) {
                Ok(mut tokens) => match parser::parse(&mut tokens) {
                    Ok(ast) => {
                        assert_eq!(ast.to_string(), $value);
                    }
                    _ => assert!(false),
                },
                _ => assert!(false),
            }
        }};
    }

    macro_rules! parsefails {
        ($input:expr, $err:tt) => {{
            match lexer::scan($input) {
                Ok(mut tokens) => match parser::parse(&mut tokens) {
                    Ok(_) => assert!(false),
                    Err(e) => assert_eq!(e.err, $err),
                },
                _ => assert!(false),
            }
        }};
    }

    #[test]
    fn parsing() {
        parse!("42", "42:Number");
        parse!("-42", "-42:Number");
        parse!("Id", "Id:Identifier");
        parse!("'hello world'", "hello world:String");
        parse!("Id", "Id:Identifier");
        parsefails!("+", "Expected value, found +.");
        parsefails!("-d", "Expected value, found -.");
        parsefails!("", "Unexpected end of input.");
        parse!("One two", "(unary One:Identifier two:Identifier)");
        parse!(
            "One two three",
            "(unary (unary One:Identifier two:Identifier) three:Identifier)"
        );
        parse!("3.14159 cos", "(unary 3.14159:Number cos:Identifier)");
        parse!(
            "'hello world' len",
            "(unary hello world:String len:Identifier)"
        );
    }
}
