use crate::lexer;
use std::collections::LinkedList;
use std::error::Error;
use std::fmt;

/*
expression     -> keyword
keyword        -> binary | binary (IDENTIFIER ":" binary)+
binary         -> unary | unary ("+"|"-"|"*"|"/"|">"|">="|"<"|"<="|"~="|"=" unary)*
unary          -> value | unary IDENTIFIER
value          -> IDENTIFIER | NUMBER | STRING | "false" | "true" | "nil"
                  | "(" expression ")"
*/

pub enum Ast {
    Keyword(Box<Ast>, Vec<(lexer::LexedToken, Ast)>),
    Binary(lexer::LexedToken, Box<Ast>, Box<Ast>),
    Unary(Box<Ast>, Box<Ast>),
    Value(lexer::LexedToken),
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ast::Keyword(obj, msg) => {
                write!(f, "(keyword {}", obj)?;
                for kw in msg {
                    write!(f, " {}: {}", kw.0.token, kw.1)?;
                }
                write!(f, ")")
            }
            Ast::Binary(op, obj, msg) => write!(f, "(binary {} {} {})", op.token, *obj, *msg),
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

macro_rules! expect {
    ($tokens:expr, $token:tt, $err:expr) => {{
        match $tokens.pop_front() {
            Some(token) => match token.token {
                lexer::Token::$token => {}
                _ => {
                    return Err(ParserError {
                        err: $err,
                        line: token.line,
                    });
                }
            },
            None => {
                return Err(ParserError {
                    err: "Unexpected end of input.".to_string(),
                    line: usize::max_value(),
                });
            }
        }
    };};
}

fn expression(tokens: &mut LinkedList<lexer::LexedToken>) -> Result<Ast, ParserError> {
    keyword(tokens)
}

fn keyword(tokens: &mut LinkedList<lexer::LexedToken>) -> Result<Ast, ParserError> {
    match binary(tokens) {
        Ok(obj) => match tokens.front() {
            Some(peek) => match peek.token {
                lexer::Token::Identifier(_) => {
                    let mut msg = Vec::new();
                    loop {
                        match tokens.front() {
                            Some(peek) => match peek.token {
                                lexer::Token::Identifier(_) => {
                                    if let Some(token) = tokens.pop_front() {
                                        expect!(tokens, Colon, "Expected :.".to_string());
                                        match binary(tokens) {
                                            Ok(value) => {
                                                msg.push((token, value));
                                            }
                                            Err(e) => {
                                                return Err(e);
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    break;
                                }
                            },
                            _ => {
                                break;
                            }
                        }
                    }
                    Ok(Ast::Keyword(Box::new(obj), msg))
                }
                _ => Ok(obj),
            },
            _ => Ok(obj),
        },
        Err(e) => Err(e),
    }
}

fn binary(tokens: &mut LinkedList<lexer::LexedToken>) -> Result<Ast, ParserError> {
    match unary(tokens) {
        Ok(mut lhs) => {
            loop {
                match tokens.front() {
                    Some(peek) => match peek.token {
                        lexer::Token::Plus
                        | lexer::Token::Minus
                        | lexer::Token::Star
                        | lexer::Token::Slash
                        | lexer::Token::Less
                        | lexer::Token::LessEqual
                        | lexer::Token::NotEqual
                        | lexer::Token::Equal
                        | lexer::Token::Greater
                        | lexer::Token::GreaterEqual => {
                            if let Some(token) = tokens.pop_front() {
                                let rhs = unary(tokens);
                                match rhs {
                                    Ok(rhs) => {
                                        lhs = Ast::Binary(token, Box::new(lhs), Box::new(rhs));
                                    }
                                    Err(e) => {
                                        return Err(e);
                                    }
                                }
                            }
                        }
                        _ => {
                            break;
                        }
                    },
                    _ => {
                        break;
                    }
                }
            }
            Ok(lhs)
        }
        Err(e) => Err(e),
    }
}

fn unary(tokens: &mut LinkedList<lexer::LexedToken>) -> Result<Ast, ParserError> {
    match value(tokens) {
        Ok(mut result) => {
            loop {
                let mut iter = tokens.iter();
                // We need two tokens of lookahead to disambigulate a unary
                // message from a keyword message.
                match iter.next() {
                    Some(peek) => {
                        match iter.next() {
                            Some(peek) => match peek.token {
                                lexer::Token::Colon => {
                                    break;
                                }
                                _ => {}
                            },
                            None => {}
                        }
                        match peek.token {
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
                        }
                    }
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
            lexer::Token::LeftParen => match expression(tokens) {
                Ok(result) => {
                    expect!(tokens, RightParen, "Expected ).".to_string());
                    Ok(result)
                }
                Err(e) => Err(e),
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
    expression(tokens)
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
                        assert_eq!(tokens.len(), 0);
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
        parse!("2 + 3", "(binary + 2:Number 3:Number)");
        parse!("2 - -3", "(binary - 2:Number -3:Number)");
        parse!("2 * 3", "(binary * 2:Number 3:Number)");
        parse!("2 / 3", "(binary / 2:Number 3:Number)");
        parse!("2 < 3", "(binary < 2:Number 3:Number)");
        parse!("2 <= 3", "(binary <= 2:Number 3:Number)");
        parse!("2 = 3", "(binary = 2:Number 3:Number)");
        parse!("2 ~= 3", "(binary ~= 2:Number 3:Number)");
        parse!("2 > 3", "(binary > 2:Number 3:Number)");
        parse!("2 >= 3", "(binary >= 2:Number 3:Number)");
        parse!(
            "4 sqrt * 3",
            "(binary * (unary 4:Number sqrt:Identifier) 3:Number)"
        );
        parse!(
            "2 + 4 sqrt * 3",
            "(binary * (binary + 2:Number (unary 4:Number sqrt:Identifier)) 3:Number)"
        );
        parse!(
            "2 * 3 * 4",
            "(binary * (binary * 2:Number 3:Number) 4:Number)"
        );
        parse!(
            "'hello ' concat: 'world'",
            "(keyword hello :String concat: world:String)"
        );
        parse!(
            "a b:1 c: 2",
            "(keyword a:Identifier b: 1:Number c: 2:Number)"
        );
        parse!(
            "a mod: 81 sqrt",
            "(keyword a:Identifier mod: (unary 81:Number sqrt:Identifier))"
        );
        // From Wikipedia example of Smalltalk message precedence
        parse!(
            "3 factorial + 4 factorial between: 10 and: 100",
            "(keyword (binary + (unary 3:Number factorial:Identifier) (unary 4:Number factorial:Identifier)) between: 10:Number and: 100:Number)"
        );
        parse!(
            "(3 factorial + 4) factorial between: 10 and: 100",
            "(keyword (unary (binary + (unary 3:Number factorial:Identifier) 4:Number) factorial:Identifier) between: 10:Number and: 100:Number)"
        );
        parse!("a b:-2", "(keyword a:Identifier b: -2:Number)");
        parse!(
            "3 + (4 * 5)",
            "(binary + 3:Number (binary * 4:Number 5:Number))"
        );
    }
}
