use crate::lexer;
use std::collections::LinkedList;
use std::error::Error;
use std::fmt;

/*
program        -> (statement)*
statement      -> IDENTIFIER ":=" expression "."
                  | expression "."
expression     -> keyword
keyword        -> binary | binary (IDENTIFIER ":" binary)+
binary         -> unary | unary ("+"|"-"|"*"|"/"|">"|">="|"<"|"<="|"~="|"=" unary)*
unary          -> value | unary IDENTIFIER
value          -> IDENTIFIER | NUMBER | STRING | "false" | "true" | "nil"
                  | "(" expression ")"
                  | "[" (statement)* "]"
                  | "[" (":" IDENTIFIER)+ "|" (statement)* "]"
                  | "[" (":" IDENTIFIER)* "|" "|" (IDENTIFIER)* "|" (statement)* "]"
*/

pub enum Ast {
    Assignment(lexer::LexedToken, Box<Ast>),
    Binary(lexer::LexedToken, Box<Ast>, Box<Ast>),
    Block(Vec<lexer::LexedToken>, Vec<lexer::LexedToken>, Vec<Ast>),
    Keyword(Box<Ast>, Vec<(lexer::LexedToken, Ast)>),
    Lookup(lexer::LexedToken),
    Program(Vec<Ast>),
    Unary(Box<Ast>, lexer::LexedToken),
    Value(lexer::LexedToken),
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ast::Assignment(id, expr) => {
                write!(f, "(assignment {}:Identifier {})", id.token, *expr)
            }
            Ast::Binary(op, obj, msg) => write!(f, "(binary {} {} {})", op.token, *obj, *msg),
            Ast::Block(params, locals, statements) => {
                write!(f, "(block")?;
                for param in params {
                    write!(f, " :{}", param.token)?;
                }
                write!(f, " |")?;
                for local in locals {
                    write!(f, " {}", local.token)?;
                }
                write!(f, " |")?;
                for statement in statements {
                    write!(f, " {}", statement)?;
                }
                write!(f, ")")
            }
            Ast::Keyword(obj, msg) => {
                write!(f, "(keyword {}", obj)?;
                for kw in msg {
                    write!(f, " {}:Identifier {}", kw.0.token, kw.1)?;
                }
                write!(f, ")")
            }
            Ast::Lookup(id) => write!(f, "(lookup {}:Identifier)", id.token),
            Ast::Program(statements) => {
                write!(f, "(program")?;
                for statement in statements {
                    write!(f, " {}", statement)?;
                }
                write!(f, ")")
            }
            Ast::Unary(obj, msg) => write!(f, "(unary {} {}:Identifier)", *obj, msg.token),
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
    pub col: usize,
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
                        col: token.col,
                    });
                }
            },
            None => {
                return Err(ParserError {
                    err: "Unexpected end of input.".to_string(),
                    line: usize::max_value(),
                    col: usize::max_value(),
                });
            }
        }
    };};
}
fn program(tokens: &mut LinkedList<lexer::LexedToken>) -> Result<Ast, ParserError> {
    let mut statements = Vec::new();
    loop {
        match tokens.front() {
            None => {
                break;
            }
            _ => {}
        }
        match statement(tokens) {
            Ok(ast) => {
                statements.push(ast);
            }
            Err(e) => {
                return Err(e);
            }
        }
    }
    Ok(Ast::Program(statements))
}

fn statement(tokens: &mut LinkedList<lexer::LexedToken>) -> Result<Ast, ParserError> {
    let result;
    match tokens.front() {
        Some(token) => match &token.token {
            lexer::Token::Identifier(_) => {
                if let Some(token) = tokens.pop_front() {
                    match tokens.front() {
                        Some(peek) => match peek.token {
                            lexer::Token::ColonEqual => {
                                tokens.pop_front();
                                match expression(tokens) {
                                    Ok(expr) => {
                                        result = Ok(Ast::Assignment(token, Box::new(expr)));
                                    }
                                    Err(e) => {
                                        return Err(e);
                                    }
                                }
                            }
                            lexer::Token::Dot => {
                                result = Ok(Ast::Lookup(token));
                            }
                            _ => {
                                tokens.push_front(token);
                                result = expression(tokens);
                            }
                        },
                        None => {
                            return Err(ParserError {
                                err: "Unexpected end of input.".to_string(),
                                line: usize::max_value(),
                                col: usize::max_value(),
                            });
                        }
                    }
                } else {
                    unreachable!();
                }
            }
            _ => match expression(tokens) {
                Ok(ast) => {
                    result = Ok(ast);
                }
                Err(e) => {
                    return Err(e);
                }
            },
        },
        None => {
            return Err(ParserError {
                err: "Unexpected end of input.".to_string(),
                line: usize::max_value(),
                col: usize::max_value(),
            });
        }
    }

    match tokens.front() {
        Some(token) => match token.token {
            lexer::Token::RightBracket => {}
            _ => {
                expect!(tokens, Dot, "Expected '.'.".to_string());
            }
        },
        None => {
            return Err(ParserError {
                err: "Unexpected end of input.".to_string(),
                line: usize::max_value(),
                col: usize::max_value(),
            });
        }
    }
    result
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
                                        expect!(tokens, Colon, "Expected ':'.".to_string());
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
                // We need two tokens of lookahead to disambiguate a unary
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
                            lexer::Token::Identifier(_) => match tokens.pop_front() {
                                Some(token) => {
                                    result = Ast::Unary(Box::new(result), token);
                                }
                                None => {
                                    unreachable!();
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
            lexer::Token::Identifier(_) => Ok(Ast::Lookup(token)),
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
                            col: token.col,
                        }));
                        tokens.pop_front();
                        return result;
                    }
                    _ => Err(ParserError {
                        err: "Expected value, found '-'.".to_string(),
                        line: token.line,
                        col: token.col,
                    }),
                },
                _ => Err(ParserError {
                    err: "Expected value, found '-'.".to_string(),
                    line: token.line,
                    col: token.col,
                }),
            },
            lexer::Token::LeftBracket => {
                let mut params = Vec::new();
                let mut locals = Vec::new();
                let mut statements = Vec::new();
                let mut expecting_bar = false;
                loop {
                    match tokens.front() {
                        Some(token) => match token.token {
                            lexer::Token::Colon => {
                                expecting_bar = true;
                                tokens.pop_front();
                                match tokens.pop_front() {
                                    Some(token) => match &token.token {
                                        lexer::Token::Identifier(_) => {
                                            params.push(token);
                                            continue;
                                        }
                                        _ => {
                                            return Err(ParserError {
                                                err: "Expected identifier after :".to_string(),
                                                line: usize::max_value(),
                                                col: usize::max_value(),
                                            });
                                        }
                                    },
                                    None => {
                                        return Err(ParserError {
                                            err: "Unexpected end of input while looking for identifier.".to_string(),
                                            line: usize::max_value(),
                                            col: usize::max_value(),
                                        });
                                    }
                                }
                            }
                            lexer::Token::Bar => {
                                tokens.pop_front();
                                break;
                            }
                            _ => {
                                if expecting_bar {
                                    return Err(ParserError {
                                        err: "Expected ':' or '|'.".to_string(),
                                        line: usize::max_value(),
                                        col: usize::max_value(),
                                    });
                                }
                                break;
                            }
                        },
                        None => {
                            break;
                        }
                    }
                }
                match tokens.front() {
                    Some(token) => match token.token {
                        lexer::Token::Bar => {
                            tokens.pop_front();
                            loop {
                                match tokens.pop_front() {
                                    Some(token) => match &token.token {
                                        lexer::Token::Identifier(_) => {
                                            locals.push(token);
                                            continue;
                                        }
                                        lexer::Token::Bar => {
                                            break;
                                        }
                                        _ => {
                                            return Err(ParserError {
                                                err: "Expected identifier.".to_string(),
                                                line: usize::max_value(),
                                                col: usize::max_value(),
                                            });
                                        }
                                    },
                                    None => {
                                        return Err(ParserError {
                                            err: "Unexpected end of input while looking for '|'."
                                                .to_string(),
                                            line: usize::max_value(),
                                            col: usize::max_value(),
                                        });
                                    }
                                }
                            }
                        }
                        _ => {}
                    },
                    None => {
                        return Err(ParserError {
                            err: "Unexpected end of input".to_string(),
                            line: usize::max_value(),
                            col: usize::max_value(),
                        });
                    }
                }
                loop {
                    match tokens.front() {
                        Some(token) => match token.token {
                            lexer::Token::RightBracket => {
                                break;
                            }
                            _ => {}
                        },
                        None => {
                            return Err(ParserError {
                                err: "Unexpected end of input while looking for ].".to_string(),
                                line: usize::max_value(),
                                col: usize::max_value(),
                            });
                        }
                    }
                    match statement(tokens) {
                        Ok(ast) => {
                            statements.push(ast);
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                expect!(tokens, RightBracket, "Expected ']'.".to_string());
                Ok(Ast::Block(params, locals, statements))
            }
            lexer::Token::LeftParen => match expression(tokens) {
                Ok(result) => {
                    expect!(tokens, RightParen, "Expected ')'.".to_string());
                    Ok(result)
                }
                Err(e) => Err(e),
            },
            _ => {
                let mut err = "Expected value, found '".to_string();
                err.push_str(&token.token.to_string());
                err.push_str("'.");
                Err(ParserError {
                    err: err,
                    line: token.line,
                    col: token.col,
                })
            }
        },
        None => {
            return Err(ParserError {
                err: "Unexpected end of input.".to_string(),
                line: usize::max_value(),
                col: usize::max_value(),
            });
        }
    }
}

pub fn parse(tokens: &mut LinkedList<lexer::LexedToken>) -> Result<Ast, ParserError> {
    program(tokens)
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
                    Err(err) => assert_eq!("parse failed", err.err),
                },
                _ => assert!(false),
            }
        }};
    }

    macro_rules! parsefails {
        ($input:expr, $err:tt, $line:expr, $col:expr) => {{
            match lexer::scan($input) {
                Ok(mut tokens) => match parser::parse(&mut tokens) {
                    Ok(_) => assert!(false),
                    Err(e) => {
                        assert_eq!(e.err, $err);
                        assert_eq!(e.line, $line);
                        assert_eq!(e.col, $col);
                    }
                },
                _ => assert!(false),
            }
        }};
    }

    #[test]
    fn parsing() {
        parse!("42.", "(program 42:Number)");
        parse!("-42.0.", "(program -42:Number)");
        parse!("Id.", "(program (lookup Id:Identifier))");
        parse!("'hello world'.", "(program hello world:String)");
        parse!("", "(program)");
        parse!(
            "One two.",
            "(program (unary (lookup One:Identifier) two:Identifier))"
        );
        parse!(
            "One two three.",
            "(program (unary (unary (lookup One:Identifier) two:Identifier) three:Identifier))"
        );
        parse!(
            "3.14159 cos.",
            "(program (unary 3.14159:Number cos:Identifier))"
        );
        parse!(
            "'hello world' len.",
            "(program (unary hello world:String len:Identifier))"
        );
        parse!("2 + 3.", "(program (binary + 2:Number 3:Number))");
        parse!("2 - -3.", "(program (binary - 2:Number -3:Number))");
        parse!("2 * 3.", "(program (binary * 2:Number 3:Number))");
        parse!("2 / 3.", "(program (binary / 2:Number 3:Number))");
        parse!("2 < 3.", "(program (binary < 2:Number 3:Number))");
        parse!("2 <= 3.", "(program (binary <= 2:Number 3:Number))");
        parse!("2 = 3.", "(program (binary = 2:Number 3:Number))");
        parse!("2 ~= 3.", "(program (binary ~= 2:Number 3:Number))");
        parse!("2 > 3.", "(program (binary > 2:Number 3:Number))");
        parse!("2 >= 3.", "(program (binary >= 2:Number 3:Number))");
        parse!(
            "4 sqrt * 3.",
            "(program (binary * (unary 4:Number sqrt:Identifier) 3:Number))"
        );
        parse!(
            "2 + 4 sqrt * 3.",
            "(program (binary * (binary + 2:Number (unary 4:Number sqrt:Identifier)) 3:Number))"
        );
        parse!(
            "2 * 3 * 4.",
            "(program (binary * (binary * 2:Number 3:Number) 4:Number))"
        );
        parse!(
            "'hello ' concat: 'world'.",
            "(program (keyword hello :String concat:Identifier world:String))"
        );
        parse!(
            "a b:1 c: 2.",
            "(program (keyword (lookup a:Identifier) b:Identifier 1:Number c:Identifier 2:Number))"
        );
        parse!(
            "a mod: 81 sqrt.",
            "(program (keyword (lookup a:Identifier) mod:Identifier (unary 81:Number sqrt:Identifier)))"
        );
        // From Wikipedia example of Smalltalk message precedence
        parse!(
            "3 factorial + 4 factorial between: 10 and: 100.",
            "(program (keyword (binary + (unary 3:Number factorial:Identifier) (unary 4:Number factorial:Identifier)) between:Identifier 10:Number and:Identifier 100:Number))"
        );
        parse!(
            "(3 factorial + 4) factorial between: 10 and: 100.",
            "(program (keyword (unary (binary + (unary 3:Number factorial:Identifier) 4:Number) factorial:Identifier) between:Identifier 10:Number and:Identifier 100:Number))"
        );
        parse!(
            "a b:-2.",
            "(program (keyword (lookup a:Identifier) b:Identifier -2:Number))"
        );
        parse!(
            "3 + (4 * 5).",
            "(program (binary + 3:Number (binary * 4:Number 5:Number)))"
        );
        parsefails!(
            "[1. 2.",
            "Unexpected end of input while looking for ].",
            usize::max_value(),
            usize::max_value()
        );
        parse!("[].", "(program (block | |))");
        parse!(
            "[1. 2. 3.].",
            "(program (block | | 1:Number 2:Number 3:Number))"
        );
        parse!("a := 3.", "(program (assignment a:Identifier 3:Number))");
        parse!(
            "a := 3 * 4 + 5.",
            "(program (assignment a:Identifier (binary + (binary * 3:Number 4:Number) 5:Number)))"
        );
        parse!(
            "[:x| x + 1 ].",
            "(program (block :x | | (binary + (lookup x:Identifier) 1:Number)))"
        );
        parse!(
            "[:x : y| x + y ].",
            "(program (block :x :y | | (binary + (lookup x:Identifier) (lookup y:Identifier))))"
        );
        parsefails!(
            "[:x x + 1.].",
            "Expected ':' or '|'.",
            usize::max_value(),
            usize::max_value()
        );
        parse!(
            "[:x||| x + 1 ].",
            "(program (block :x | | (binary + (lookup x:Identifier) 1:Number)))"
        );
        parse!(
            "[:x||y| x + y + 1 ].",
            "(program (block :x | y | (binary + (binary + (lookup x:Identifier) (lookup y:Identifier)) 1:Number)))"
        );
    }
}
