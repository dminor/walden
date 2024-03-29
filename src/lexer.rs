use std::collections::LinkedList;
use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Token {
    Bar,
    Colon,
    ColonEqual,
    Dot,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Star,
    Slash,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,

    Identifier(String),
    Number(f64),
    String(String),

    True,
    False,
    Nil,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Bar => write!(f, "|"),
            Token::Colon => write!(f, ":"),
            Token::ColonEqual => write!(f, ":="),
            Token::Dot => write!(f, "."),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::NotEqual => write!(f, "~="),
            Token::Equal => write!(f, "="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::String(s) => write!(f, "{}", s),
            Token::Number(n) => write!(f, "{}", n),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct LexerError {
    pub err: String,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LexerError: {}", self.err)
    }
}

impl Error for LexerError {}

pub struct LexedToken {
    pub token: Token,
    pub line: usize,
    pub col: usize,
}

macro_rules! push_token {
    ($T:expr, $tokens:ident, $line:ident, $col:ident) => {
        $tokens.push_back({
            LexedToken {
                token: $T,
                line: $line,
                col: $col,
            }
        });
    };
}

pub fn scan(src: &str) -> Result<LinkedList<LexedToken>, LexerError> {
    let mut line = 1;
    let mut col = 1;
    let mut tokens = LinkedList::<LexedToken>::new();
    let mut chars = src.chars().peekable();
    loop {
        match chars.next() {
            Some(c) => match c {
                '|' => {
                    push_token!(Token::Bar, tokens, line, col);
                }
                ':' => match chars.peek() {
                    Some(c) => {
                        if *c == '=' {
                            push_token!(Token::ColonEqual, tokens, line, col);
                            chars.next();
                        } else {
                            push_token!(Token::Colon, tokens, line, col);
                        }
                    }
                    None => {
                        push_token!(Token::Colon, tokens, line, col);
                    }
                },
                '.' => {
                    push_token!(Token::Dot, tokens, line, col);
                }
                '[' => {
                    push_token!(Token::LeftBracket, tokens, line, col);
                }
                ']' => {
                    push_token!(Token::RightBracket, tokens, line, col);
                }
                '(' => {
                    push_token!(Token::LeftParen, tokens, line, col);
                }
                ')' => {
                    push_token!(Token::RightParen, tokens, line, col);
                }
                '+' => {
                    push_token!(Token::Plus, tokens, line, col);
                }
                '-' => {
                    push_token!(Token::Minus, tokens, line, col);
                }
                '*' => {
                    push_token!(Token::Star, tokens, line, col);
                }
                '/' => {
                    push_token!(Token::Slash, tokens, line, col);
                }
                '>' => match chars.peek() {
                    Some(c) => {
                        if *c == '=' {
                            push_token!(Token::GreaterEqual, tokens, line, col);
                            chars.next();
                            col += 1;
                        } else {
                            push_token!(Token::Greater, tokens, line, col);
                        }
                    }
                    None => {
                        push_token!(Token::Greater, tokens, line, col);
                    }
                },
                '=' => {
                    push_token!(Token::Equal, tokens, line, col);
                }
                '<' => match chars.peek() {
                    Some(c) => {
                        if *c == '=' {
                            push_token!(Token::LessEqual, tokens, line, col);
                            chars.next();
                            col += 1;
                        } else {
                            push_token!(Token::Less, tokens, line, col);
                        }
                    }
                    None => {
                        push_token!(Token::Less, tokens, line, col);
                    }
                },
                '~' => match chars.next() {
                    Some(c) => {
                        if c == '=' {
                            col += 1;
                            push_token!(Token::NotEqual, tokens, line, col);
                        } else {
                            return Err(LexerError {
                                err: "Unexpected token while scanning ~=.".to_string(),
                                line: line,
                                col: col,
                            });
                        }
                    }
                    _ => {
                        return Err(LexerError {
                            err: "Unexpected end of input while scanning ~=.".to_string(),
                            line: line,
                            col: col,
                        });
                    }
                },
                '\'' => {
                    let mut v = Vec::<char>::new();
                    loop {
                        match chars.next() {
                            Some(c) => match c {
                                '\'' => {
                                    push_token!(
                                        Token::String(v.into_iter().collect()),
                                        tokens,
                                        line,
                                        col
                                    );
                                    break;
                                }
                                '\n' => {
                                    line += 1;
                                    col = 1;
                                    v.push(c);
                                }
                                _ => {
                                    v.push(c);
                                    col += 1;
                                }
                            },
                            None => {
                                return Err(LexerError {
                                    err: "Unexpected end of input while scanning string."
                                        .to_string(),
                                    line: line,
                                    col: col,
                                });
                            }
                        }
                    }
                    col += 1;
                }
                '"' => loop {
                    match chars.next() {
                        Some(c) => match c {
                            '"' => {
                                break;
                            }
                            '\n' => {
                                line += 1;
                                col = 0; //incremented below
                            }
                            _ => {}
                        },
                        None => {
                            return Err(LexerError {
                                err: "Unexpected end of input while scanning comment.".to_string(),
                                line: line,
                                col: col,
                            });
                        }
                    }
                    col += 1;
                },
                '\n' => {
                    line += 1;
                    col = 1;
                    continue;
                }
                ' ' => {}
                _ => {
                    let mut valid_identifier = true;
                    let mut found_dot = false;
                    let mut push_dot = false;
                    if c.is_numeric() {
                        valid_identifier = false;
                    }
                    if !c.is_alphanumeric() && c != '@' {
                        valid_identifier = false;
                    }
                    let mut v = vec![c];
                    loop {
                        match chars.peek() {
                            Some(c) => {
                                if c.is_alphanumeric() {
                                    v.push(*c);
                                    chars.next();
                                    col += 1;
                                } else if !found_dot && *c == '.' {
                                    found_dot = true;
                                    chars.next();
                                    col += 1;
                                    match chars.peek() {
                                        Some(c) => {
                                            if c.is_numeric() {
                                                valid_identifier = false;
                                                v.push('.');
                                            } else {
                                                push_dot = true;
                                                break;
                                            }
                                        }
                                        None => {
                                            push_dot = true;
                                            break;
                                        }
                                    }
                                } else {
                                    break;
                                }
                            }
                            None => {
                                break;
                            }
                        }
                    }
                    let s: String = v.into_iter().collect();
                    match &s[..] {
                        "true" => {
                            push_token!(Token::True, tokens, line, col);
                        }
                        "false" => {
                            push_token!(Token::False, tokens, line, col);
                        }
                        "nil" => {
                            push_token!(Token::Nil, tokens, line, col);
                        }
                        _ => match s.parse::<f64>() {
                            Ok(n) => {
                                push_token!(Token::Number(n), tokens, line, col);
                            }
                            _ => {
                                if valid_identifier {
                                    push_token!(
                                        Token::Identifier(s.to_string()),
                                        tokens,
                                        line,
                                        col
                                    );
                                } else {
                                    let mut err = "Invalid identifier: ".to_string();
                                    err.push_str(&s);
                                    err.push('.');
                                    return Err(LexerError {
                                        err: err,
                                        line: line,
                                        col: col,
                                    });
                                }
                            }
                        },
                    }
                    if push_dot {
                        push_token!(Token::Dot, tokens, line, col);
                    }
                }
            },
            None => {
                break;
            }
        }
        col += 1;
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use crate::lexer;

    macro_rules! scan {
        ($input:expr, $( $value:expr),* ) => {{
            match lexer::scan($input) {
                Ok(mut tokens) => {
                    $(
                        match tokens.pop_front() {
                            Some(t) => {
                                assert_eq!(t.token, $value);
                            }
                            None => {}
                        }
                    )*
                    assert_eq!(tokens.len(), 0);
                }
                _ => assert!(false),
            }
        }};
    }

    macro_rules! scanfails {
        ($input:expr, $err:tt, $line:expr, $col:expr) => {{
            match lexer::scan($input) {
                Ok(_) => assert!(false),
                Err(e) => {
                    assert_eq!(e.err, $err);
                    assert_eq!(e.line, $line);
                    assert_eq!(e.col, $col);
                }
            }
        }};
    }

    #[test]
    fn scanning() {
        scan!(
            "[:x | x + 1]",
            lexer::Token::LeftBracket,
            lexer::Token::Colon,
            lexer::Token::Identifier("x".to_string()),
            lexer::Token::Bar,
            lexer::Token::Identifier("x".to_string()),
            lexer::Token::Plus,
            lexer::Token::Number(1.0),
            lexer::Token::RightBracket
        );

        scan!(
            "[:x :y | x - y]",
            lexer::Token::LeftBracket,
            lexer::Token::Colon,
            lexer::Token::Identifier("x".to_string()),
            lexer::Token::Colon,
            lexer::Token::Identifier("y".to_string()),
            lexer::Token::Bar,
            lexer::Token::Identifier("x".to_string()),
            lexer::Token::Minus,
            lexer::Token::Identifier("y".to_string()),
            lexer::Token::RightBracket
        );

        scan!(
            "5 * 6 / 2.2",
            lexer::Token::Number(5.0),
            lexer::Token::Star,
            lexer::Token::Number(6.0),
            lexer::Token::Slash,
            lexer::Token::Number(2.2)
        );

        scan!(
            "(x <= y) or: true",
            lexer::Token::LeftParen,
            lexer::Token::Identifier("x".to_string()),
            lexer::Token::LessEqual,
            lexer::Token::Identifier("y".to_string()),
            lexer::Token::RightParen,
            lexer::Token::Identifier("or".to_string()),
            lexer::Token::Colon,
            lexer::Token::True
        );

        scan!(
            "(x >= y) and: false",
            lexer::Token::LeftParen,
            lexer::Token::Identifier("x".to_string()),
            lexer::Token::GreaterEqual,
            lexer::Token::Identifier("y".to_string()),
            lexer::Token::RightParen,
            lexer::Token::Identifier("and".to_string()),
            lexer::Token::Colon,
            lexer::Token::False
        );

        scan!(
            "2 ~= 3 = true",
            lexer::Token::Number(2.0),
            lexer::Token::NotEqual,
            lexer::Token::Number(3.0),
            lexer::Token::Equal,
            lexer::Token::True
        );

        scanfails!("2 ~$", "Unexpected token while scanning ~=.", 1, 3);

        scanfails!("2 ~", "Unexpected end of input while scanning ~=.", 1, 3);

        scan!("<", lexer::Token::Less);
        scan!(">", lexer::Token::Greater);
        scan!("nil", lexer::Token::Nil);

        scan!("Valid", lexer::Token::Identifier("Valid".to_string()));
        scan!("@Valid", lexer::Token::Identifier("@Valid".to_string()));
        scanfails!("2Valid", "Invalid identifier: 2Valid.", 1, 6);

        scanfails!("In_valid", "Invalid identifier: _valid.", 1, 8);

        scanfails!("$valid", "Invalid identifier: $valid.", 1, 6);

        scan!(
            "'hello world'",
            lexer::Token::String("hello world".to_string())
        );

        scan!(
            "'hello\nworld'",
            lexer::Token::String("hello\nworld".to_string())
        );

        scanfails!(
            "'hello world",
            "Unexpected end of input while scanning string.",
            1,
            12
        );

        scanfails!(
            "\"This is a comment that\nnever ends",
            "Unexpected end of input while scanning comment.",
            2,
            11
        );
    }
}
