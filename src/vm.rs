use crate::stdlib;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub struct RuntimeError {
    pub err: String,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for RuntimeError {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RuntimeError: {}", self.err)
    }
}

impl Error for RuntimeError {}

#[derive(Default)]
pub struct Object {
    pub prototype: Option<Rc<RefCell<Object>>>,
    pub members: HashMap<String, Value>,
}

impl Object {
    pub fn lookup(&self, member: String) -> Option<Value> {
        if member.starts_with('@') {
            return match self.lookup("self".to_string()) {
                Some(Value::Object(this)) => this.borrow().lookup(member[1..].to_string()),
                _ => None,
            };
        }
        match self.members.get(&member) {
            Some(v) => match v {
                Value::Block(proto, params, locals, address) => Some(Value::Block(
                    proto.clone(),
                    params.to_vec(),
                    locals.to_vec(),
                    *address,
                )),
                Value::Boolean(proto, b) => Some(Value::Boolean(proto.clone(), *b)),
                Value::Nil(proto) => Some(Value::Nil(proto.clone())),
                Value::Number(proto, n) => Some(Value::Number(proto.clone(), *n)),
                Value::Object(obj) => Some(Value::Object(obj.clone())),
                Value::RustBlock(proto, name, f) => {
                    Some(Value::RustBlock(proto.clone(), name.to_string(), *f))
                }
                Value::String(proto, s) => Some(Value::String(proto.clone(), s.to_string())),
            },
            None => match &self.prototype {
                Some(proto) => proto.borrow().lookup(member),
                None => None,
            },
        }
    }

    pub fn new() -> Object {
        Object {
            prototype: None,
            members: HashMap::new(),
        }
    }

    pub fn new_with_prototype(proto: Rc<RefCell<Object>>) -> Object {
        Object {
            prototype: Some(proto.clone()),
            members: HashMap::new(),
        }
    }

    pub fn override_with(&mut self, member: String, value: Value) -> () {
        if member.starts_with('@') {
            return match self.lookup("self".to_string()) {
                Some(Value::Object(this)) => this
                    .borrow_mut()
                    .override_with(member[1..].to_string(), value),
                _ => {}
            };
        }
        self.members.insert(member, value.clone());
    }

    pub fn set_to(&mut self, member: String, value: Value) -> () {
        if member.starts_with('@') {
            return match self.lookup("self".to_string()) {
                Some(Value::Object(this)) => {
                    this.borrow_mut().set_to(member[1..].to_string(), value)
                }
                _ => {}
            };
        }

        if self.members.contains_key(&member) {
            self.members.insert(member, value.clone());
            return;
        }

        if let Some(mut borrowed) = Some(self.prototype.clone()) {
            loop {
                match borrowed {
                    Some(proto) => {
                        if proto.borrow().members.contains_key(&member) {
                            proto.borrow_mut().members.insert(member, value.clone());
                            return;
                        }
                        borrowed = proto.borrow().prototype.clone();
                    }
                    None => {
                        break;
                    }
                }
            }
        }

        self.members.insert(member, value.clone());
    }
}

#[derive(Clone)]
pub enum Value {
    Block(Rc<RefCell<Object>>, Vec<String>, Vec<String>, usize),
    Boolean(Rc<RefCell<Object>>, bool),
    Nil(Rc<RefCell<Object>>),
    Number(Rc<RefCell<Object>>, f64),
    Object(Rc<RefCell<Object>>),
    RustBlock(
        Rc<RefCell<Object>>,
        String,
        fn(&mut VirtualMachine) -> Result<(), RuntimeError>,
    ),
    String(Rc<RefCell<Object>>, String),
}

impl fmt::Display for Value {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Block(proto, _, _, u) => write!(f, "(lambda @{} proto {:p})", u, proto),
            Value::Boolean(_, b) => write!(f, "{}", b),
            Value::Nil(_) => write!(f, "nil"),
            Value::Number(_, n) => write!(f, "{}", n),
            Value::Object(obj) => write!(f, "(object {:p})", *obj),
            Value::RustBlock(_, name, _) => write!(f, "(lambda {})", name),
            Value::String(_, s) => write!(f, "'{}'", s),
        }
    }
}

pub enum Opcode {
    Add,
    Div,
    Const(Value),
    Mul,
    Sub,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Call,
    Dup,
    Lookup,
    Pop,
    Ret,
    Srcpos(usize, usize),
    Swap,
    This,
}

impl fmt::Display for Opcode {
    fn fmt<'a>(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Opcode::Add => write!(f, "add"),
            Opcode::Div => write!(f, "div"),
            Opcode::Mul => write!(f, "mul"),
            Opcode::Const(obj) => write!(f, "const {}", obj),
            Opcode::Sub => write!(f, "sub"),
            Opcode::Less => write!(f, "lt"),
            Opcode::LessEqual => write!(f, "le"),
            Opcode::Equal => write!(f, "eq"),
            Opcode::NotEqual => write!(f, "ne"),
            Opcode::Greater => write!(f, "gt"),
            Opcode::GreaterEqual => write!(f, "ge"),
            Opcode::Call => write!(f, "call"),
            Opcode::Dup => write!(f, "dup"),
            Opcode::Lookup => write!(f, "lookup"),
            Opcode::Pop => write!(f, "pop"),
            Opcode::Ret => write!(f, "ret"),
            Opcode::Srcpos(line, col) => write!(f, "srcpos {} {}", line, col),
            Opcode::Swap => write!(f, "swap"),
            Opcode::This => write!(f, "this"),
        }
    }
}

#[derive(Default)]
pub struct VirtualMachine {
    pub callstack: Vec<(Value, usize, usize, bool)>,
    pub instructions: Vec<Opcode>,
    pub ip: usize,
    pub stack: Vec<Value>,

    // Prototypes for builtin objects
    pub object: Rc<RefCell<Object>>,
    pub block: Rc<RefCell<Object>>,
    pub boolean: Rc<RefCell<Object>>,
    pub iterator: Rc<RefCell<Object>>,
    pub number: Rc<RefCell<Object>>,
    pub string: Rc<RefCell<Object>>,

    pub enable_tracing: bool,

    pub line: usize,
    pub col: usize,
}

macro_rules! apply_op {
    ($self:tt, $in:tt, $out:tt, $proto:expr, $rustop:tt, $opcode:tt) => (
        match $self.stack.pop() {
            Some(Value::$in(_, a)) => match $self.stack.pop() {
                Some(Value::$in(_, b)) => {
                    $self.stack.push(Value::$out($proto, b $rustop a));
                }
                None => {
                    return Err(RuntimeError {
                        err: "Stack underflow.".to_string(),
                        line: $self.line,
                        col: $self.col,
                    });
                }
                _ => {
                    let mut err = "Unsupported types for ".to_string();
                    err.push_str(&Opcode::$opcode.to_string());
                    err.push('.');
                    return Err(RuntimeError {
                        err: err,
                        line: $self.line,
                        col: $self.col,
                    });
                }
            },
            None => {
                return Err(RuntimeError {
                    err: "Stack underflow.".to_string(),
                    line: $self.line,
                    col: $self.col,
                });
            }
            _ => {
                let mut err = "Unsupported types for ".to_string();
                err.push_str(&Opcode::$opcode.to_string());
                err.push('.');
                return Err(RuntimeError {
                    err: err,
                    line: $self.line,
                    col: $self.col,
                });
            }
        }
    );
}

macro_rules! apply_eq {
    ($self:tt, $in:tt) => {
        match $self.stack.pop() {
            Some(Value::$in(_, a)) => match $self.stack.pop() {
                Some(Value::$in(_, b)) => {
                    $self
                        .stack
                        .push(Value::Boolean($self.boolean.clone(), b == a));
                }
                None => {
                    return Err(RuntimeError {
                        err: "Stack underflow.".to_string(),
                        line: $self.line,
                        col: $self.col,
                    });
                }
                _ => {
                    $self
                        .stack
                        .push(Value::Boolean($self.boolean.clone(), false));
                }
            },
            None => {
                return Err(RuntimeError {
                    err: "Stack underflow.".to_string(),
                    line: $self.line,
                    col: $self.col,
                });
            }
            _ => unreachable!(),
        }
    };
}

macro_rules! apply_neq {
    ($self:tt, $in:tt) => {
        match $self.stack.pop() {
            Some(Value::$in(_, a)) => match $self.stack.pop() {
                Some(Value::$in(_, b)) => {
                    $self
                        .stack
                        .push(Value::Boolean($self.boolean.clone(), b != a));
                }
                None => {
                    return Err(RuntimeError {
                        err: "Stack underflow.".to_string(),
                        line: $self.line,
                        col: $self.col,
                    });
                }
                _ => {
                    $self
                        .stack
                        .push(Value::Boolean($self.boolean.clone(), true));
                }
            },
            None => {
                return Err(RuntimeError {
                    err: "Stack underflow.".to_string(),
                    line: $self.line,
                    col: $self.col,
                });
            }
            _ => unreachable!(),
        }
    };
}

impl VirtualMachine {
    pub fn assemble(&mut self, code: &str) -> Result<(), RuntimeError> {
        let mut lineno = 1;
        for line in code.split('\n') {
            let split: Vec<&str> = line.trim().split_whitespace().collect();
            if split.is_empty() {
                lineno += 1;
                continue;
            }

            if let Some(';') = split[0].chars().next() {
                lineno += 1;
                continue;
            }

            match split[0] {
                "const" => {
                    if split.len() >= 2 {
                        if split[1] == "false" {
                            self.instructions
                                .push(Opcode::Const(Value::Boolean(self.boolean.clone(), false)));
                        } else if split[1] == "nil" {
                            self.instructions
                                .push(Opcode::Const(Value::Nil(self.object.clone())));
                        } else if split[1] == "true" {
                            self.instructions
                                .push(Opcode::Const(Value::Boolean(self.boolean.clone(), true)));
                        } else {
                            if let Some('\'') = split[1].chars().next() {
                                let mut string = split[1][1..].to_string();
                                for s in &split[2..] {
                                    string.push(' ');
                                    string.push_str(s);
                                }

                                self.instructions.push(Opcode::Const(Value::String(
                                    self.string.clone(),
                                    string,
                                )));
                            } else {
                                match split[1].parse::<f64>() {
                                    Ok(n) => {
                                        self.instructions.push(Opcode::Const(Value::Number(
                                            self.number.clone(),
                                            n,
                                        )));
                                    }
                                    _ => {
                                        return Err(RuntimeError {
                                            err: "Invalid constant.".to_string(),
                                            line: lineno,
                                            col: 1,
                                        });
                                    }
                                }
                            }
                        }
                    } else {
                        return Err(RuntimeError {
                            err: "Missing constant argument.".to_string(),
                            line: lineno,
                            col: 1,
                        });
                    }
                }
                "add" => {
                    self.instructions.push(Opcode::Add);
                }
                "div" => {
                    self.instructions.push(Opcode::Div);
                }
                "mul" => {
                    self.instructions.push(Opcode::Mul);
                }
                "sub" => {
                    self.instructions.push(Opcode::Sub);
                }
                "lt" => {
                    self.instructions.push(Opcode::Less);
                }
                "le" => {
                    self.instructions.push(Opcode::LessEqual);
                }
                "eq" => {
                    self.instructions.push(Opcode::Equal);
                }
                "ne" => {
                    self.instructions.push(Opcode::NotEqual);
                }
                "gt" => {
                    self.instructions.push(Opcode::Greater);
                }
                "ge" => {
                    self.instructions.push(Opcode::GreaterEqual);
                }
                "block" => {
                    if split.len() >= 2 {
                        match split[split.len() - 1].parse::<usize>() {
                            Ok(ip) => {
                                let mut params = Vec::new();
                                for i in 1..split.len() - 1 {
                                    params.push(split[i].to_string());
                                }
                                self.instructions.push(Opcode::Const(Value::Block(
                                    self.object.clone(),
                                    params,
                                    Vec::new(),
                                    ip,
                                )));
                            }
                            Err(_) => {
                                return Err(RuntimeError {
                                    err: "block requires ip.".to_string(),
                                    line: lineno,
                                    col: 1,
                                });
                            }
                        }
                    } else {
                        return Err(RuntimeError {
                            err: "block requires ip.".to_string(),
                            line: lineno,
                            col: 1,
                        });
                    }
                }
                "call" => {
                    self.instructions.push(Opcode::Call);
                }
                "dup" => {
                    self.instructions.push(Opcode::Dup);
                }
                "lookup" => {
                    self.instructions.push(Opcode::Lookup);
                }
                "pop" => {
                    self.instructions.push(Opcode::Pop);
                }
                "ret" => {
                    self.instructions.push(Opcode::Ret);
                }
                "srcpos" => {
                    if split.len() >= 3 {
                        match split[1].parse::<usize>() {
                            Ok(line) => match split[2].parse::<usize>() {
                                Ok(col) => {
                                    self.instructions.push(Opcode::Srcpos(line, col));
                                }
                                _ => {
                                    return Err(RuntimeError {
                                        err: "srcpos invalid col.".to_string(),
                                        line: lineno,
                                        col: 1,
                                    });
                                }
                            },
                            _ => {
                                return Err(RuntimeError {
                                    err: "srcpos invalid line.".to_string(),
                                    line: lineno,
                                    col: 1,
                                });
                            }
                        }
                    } else {
                        return Err(RuntimeError {
                            err: "srcpos requires line and col.".to_string(),
                            line: lineno,
                            col: 1,
                        });
                    }
                }
                "swap" => {
                    self.instructions.push(Opcode::Swap);
                }
                "this" => {
                    self.instructions.push(Opcode::This);
                }
                _ => {
                    return Err(RuntimeError {
                        err: "Invalid instruction.".to_string(),
                        line: lineno,
                        col: 1,
                    });
                }
            }
            lineno += 1;
        }

        Ok(())
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        while self.ip < self.instructions.len() {
            if self.enable_tracing {
                println!("{}: {}", self.ip, self.instructions[self.ip]);
            }
            match &self.instructions[self.ip] {
                Opcode::Add => apply_op!(self, Number, Number, self.number.clone(), +, Add),
                Opcode::Const(obj) => match obj {
                    Value::Block(proto, params, locals, ip) => match self.callstack.last() {
                        Some((Value::Block(proto, _, _, _), _, _, _)) => {
                            self.stack.push(Value::Block(
                                Rc::new(RefCell::new(Object::new_with_prototype(proto.clone()))),
                                params.to_vec(),
                                locals.to_vec(),
                                *ip,
                            ));
                        }
                        _ => {
                            self.stack.push(Value::Block(
                                Rc::new(RefCell::new(Object::new_with_prototype(proto.clone()))),
                                params.to_vec(),
                                locals.to_vec(),
                                *ip,
                            ));
                        }
                    },
                    Value::Boolean(proto, b) => {
                        self.stack.push(Value::Boolean(proto.clone(), *b));
                    }
                    Value::Nil(proto) => {
                        self.stack.push(Value::Nil(proto.clone()));
                    }
                    Value::Number(proto, n) => {
                        self.stack.push(Value::Number(proto.clone(), *n));
                    }
                    Value::Object(_) => {
                        unreachable!();
                    }
                    Value::RustBlock(_, _, _) => {
                        unreachable!();
                    }
                    Value::String(proto, s) => {
                        self.stack.push(Value::String(proto.clone(), s.to_string()));
                    }
                },
                Opcode::Div => apply_op!(self, Number, Number, self.number.clone(),  /, Div),
                Opcode::Mul => apply_op!(self, Number, Number, self.number.clone(),  *, Mul),
                Opcode::Sub => apply_op!(self, Number, Number, self.number.clone(),  -, Sub),
                Opcode::Less => apply_op!(self, Number, Boolean, self.boolean.clone(),  <, Less),
                Opcode::LessEqual => {
                    apply_op!(self, Number, Boolean, self.boolean.clone(), <=, LessEqual)
                }
                Opcode::Equal => match self.stack.last() {
                    Some(Value::Block(_, _, _, _)) => apply_eq!(self, Boolean),
                    Some(Value::Boolean(_, _)) => apply_eq!(self, Boolean),
                    Some(Value::Number(_, _)) => apply_eq!(self, Number),
                    Some(Value::Object(_)) => match self.stack.pop() {
                        Some(Value::Object(a)) => match self.stack.pop() {
                            Some(Value::Object(b)) => {
                                self.stack
                                    .push(Value::Boolean(self.boolean.clone(), Rc::ptr_eq(&a, &b)));
                            }
                            None => {
                                return Err(RuntimeError {
                                    err: "Stack underflow.".to_string(),
                                    line: self.line,
                                    col: self.col,
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), false));
                            }
                        },
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: self.line,
                                col: self.col,
                            });
                        }
                        _ => unreachable!(),
                    },
                    Some(Value::Nil(_)) => match self.stack.pop() {
                        Some(Value::Nil(_)) => match self.stack.pop() {
                            Some(Value::Nil(_)) => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), true));
                            }
                            None => {
                                return Err(RuntimeError {
                                    err: "Stack underflow.".to_string(),
                                    line: self.line,
                                    col: self.col,
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), false));
                            }
                        },
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: self.line,
                                col: self.col,
                            });
                        }
                        _ => unreachable!(),
                    },

                    Some(Value::RustBlock(_, _, _)) => match self.stack.pop() {
                        Some(Value::RustBlock(_, _, _)) => match self.stack.pop() {
                            None => {
                                return Err(RuntimeError {
                                    err: "Stack underflow.".to_string(),
                                    line: self.line,
                                    col: self.col,
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), false));
                            }
                        },
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: self.line,
                                col: self.col,
                            });
                        }
                        _ => unreachable!(),
                    },
                    Some(Value::String(_, _)) => apply_eq!(self, String),
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: self.line,
                            col: self.col,
                        });
                    }
                },
                Opcode::NotEqual => match self.stack.last() {
                    Some(Value::Block(_, _, _, _)) => apply_neq!(self, Boolean),
                    Some(Value::Boolean(_, _)) => apply_neq!(self, Boolean),
                    Some(Value::Number(_, _)) => apply_neq!(self, Number),
                    Some(Value::Object(_)) => match self.stack.pop() {
                        Some(Value::Object(a)) => match self.stack.pop() {
                            Some(Value::Object(b)) => {
                                self.stack.push(Value::Boolean(
                                    self.boolean.clone(),
                                    !Rc::ptr_eq(&a, &b),
                                ));
                            }
                            None => {
                                return Err(RuntimeError {
                                    err: "Stack underflow.".to_string(),
                                    line: self.line,
                                    col: self.col,
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), false));
                            }
                        },
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: self.line,
                                col: self.col,
                            });
                        }
                        _ => unreachable!(),
                    },
                    Some(Value::Nil(_)) => match self.stack.pop() {
                        Some(Value::Nil(_)) => match self.stack.pop() {
                            Some(Value::Nil(_)) => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), false));
                            }
                            None => {
                                return Err(RuntimeError {
                                    err: "Stack underflow.".to_string(),
                                    line: self.line,
                                    col: self.col,
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), true));
                            }
                        },
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: self.line,
                                col: self.col,
                            });
                        }
                        _ => {
                            self.stack.push(Value::Boolean(self.boolean.clone(), true));
                        }
                    },
                    Some(Value::RustBlock(_, _, _)) => match self.stack.pop() {
                        Some(Value::RustBlock(_, _, _)) => match self.stack.pop() {
                            None => {
                                return Err(RuntimeError {
                                    err: "Stack underflow.".to_string(),
                                    line: self.line,
                                    col: self.col,
                                });
                            }
                            _ => {
                                self.stack.push(Value::Boolean(self.boolean.clone(), true));
                            }
                        },
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: self.line,
                                col: self.col,
                            });
                        }
                        _ => unreachable!(),
                    },
                    Some(Value::String(_, _)) => apply_neq!(self, String),
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: self.line,
                            col: self.col,
                        });
                    }
                },
                Opcode::Greater => {
                    apply_op!(self, Number, Boolean, self.boolean.clone(), >, Greater)
                }
                Opcode::GreaterEqual => {
                    apply_op!(self, Number, Boolean, self.boolean.clone(), >=, GreaterEqual)
                }
                Opcode::Call => match self.stack.pop() {
                    Some(Value::Block(proto, params, locals, ip)) => {
                        match self.stack.pop() {
                            Some(value) => match value {
                                Value::Block(this, _, _, _)
                                | Value::Boolean(this, _)
                                | Value::Nil(this)
                                | Value::Number(this, _)
                                | Value::Object(this)
                                | Value::RustBlock(this, _, _)
                                | Value::String(this, _) => {
                                    proto
                                        .borrow_mut()
                                        .members
                                        .insert("self".to_string(), Value::Object(this.clone()));
                                }
                            },
                            None => {
                                proto
                                    .borrow_mut()
                                    .members
                                    .insert("self".to_string(), Value::Object(self.block.clone()));
                            }
                        }
                        let proto =
                            Rc::new(RefCell::new(Object::new_with_prototype(proto.clone())));
                        for param in params.iter() {
                            match self.stack.pop() {
                                Some(value) => {
                                    proto.borrow_mut().members.insert(param.to_string(), value);
                                }
                                None => {
                                    return Err(RuntimeError {
                                        err: "Stack underflow.".to_string(),
                                        line: self.line,
                                        col: self.col,
                                    });
                                }
                            }
                        }
                        for local in locals.iter() {
                            proto
                                .borrow_mut()
                                .members
                                .insert(local.to_string(), Value::Nil(self.object.clone()));
                        }
                        self.callstack.push((
                            Value::Block(proto.clone(), params.to_vec(), locals.to_vec(), ip),
                            self.stack.len(),
                            self.ip + 1,
                            false,
                        ));
                        self.ip = ip;
                        continue;
                    }
                    Some(Value::RustBlock(_, _, lambda)) => {
                        let ip = self.ip;
                        match lambda(self) {
                            Ok(()) => {
                                // skip incrementing ip if the block
                                // manipulated it
                                if ip != self.ip {
                                    continue;
                                }
                            }
                            Err(err) => {
                                return Err(err);
                            }
                        }
                    }
                    Some(Value::Nil(_)) => {
                        return Err(RuntimeError {
                            err: "Message not understood.".to_string(),
                            line: self.line,
                            col: self.col,
                        });
                    }
                    _ => {
                        return Err(RuntimeError {
                            err: "Attempt to call non-lambda value.".to_string(),
                            line: self.line,
                            col: self.col,
                        });
                    }
                },
                Opcode::Dup => match self.stack.pop() {
                    Some(a) => {
                        self.stack.push(a.clone());
                        self.stack.push(a);
                    }
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: self.line,
                            col: self.col,
                        });
                    }
                },
                Opcode::Lookup => match self.stack.pop() {
                    Some(Value::String(_, s)) => match self.stack.pop() {
                        Some(Value::Block(proto, _, _, _))
                        | Some(Value::Boolean(proto, _))
                        | Some(Value::Nil(proto))
                        | Some(Value::Number(proto, _))
                        | Some(Value::RustBlock(proto, _, _))
                        | Some(Value::String(proto, _)) => {
                            let result;
                            match proto.borrow().lookup(s) {
                                Some(v) => {
                                    result = v;
                                }
                                None => {
                                    result = Value::Nil(self.object.clone());
                                }
                            }
                            self.stack.push(result);
                        }
                        Some(Value::Object(obj)) => {
                            let result;
                            match obj.borrow().lookup(s) {
                                Some(v) => {
                                    result = v;
                                }
                                None => {
                                    result = Value::Nil(self.object.clone());
                                }
                            }
                            self.stack.push(result);
                        }
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: self.line,
                                col: self.col,
                            });
                        }
                    },
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: self.line,
                            col: self.col,
                        });
                    }
                    _ => {
                        return Err(RuntimeError {
                            err: "Lookup expects string.".to_string(),
                            line: self.line,
                            col: self.col,
                        });
                    }
                },
                Opcode::Pop => match self.stack.pop() {
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: self.line,
                            col: self.col,
                        });
                    }
                    _ => {}
                },
                Opcode::Ret => match self.callstack.pop() {
                    Some((_, _, ip, brk)) => {
                        self.ip = ip;
                        if brk {
                            break;
                        }
                        continue; // skip incrementing ip
                    }
                    None => {
                        return Err(RuntimeError {
                            err: "Call stack underflow.".to_string(),
                            line: self.line,
                            col: self.col,
                        });
                    }
                },
                Opcode::Srcpos(line, col) => {
                    self.line = *line;
                    self.col = *col;
                }
                Opcode::Swap => match self.stack.pop() {
                    Some(a) => match self.stack.pop() {
                        Some(b) => {
                            self.stack.push(a);
                            self.stack.push(b);
                        }
                        None => {
                            return Err(RuntimeError {
                                err: "Stack underflow.".to_string(),
                                line: self.line,
                                col: self.col,
                            });
                        }
                    },
                    None => {
                        return Err(RuntimeError {
                            err: "Stack underflow.".to_string(),
                            line: self.line,
                            col: self.col,
                        });
                    }
                },
                Opcode::This => match self.callstack.last() {
                    Some((obj, _, _, _)) => {
                        self.stack.push(obj.clone());
                    }
                    None => {
                        self.stack.push(Value::Object(self.block.clone()));
                    }
                },
            }
            if self.enable_tracing {
                for i in 0..self.stack.len() {
                    println!("  stack {}: {}", i, self.stack[self.stack.len() - 1 - i]);
                }
            }
            self.ip += 1;
        }
        Ok(())
    }

    pub fn new() -> VirtualMachine {
        let object = Rc::new(RefCell::new(Object::new()));

        let mut vm = VirtualMachine {
            callstack: Vec::new(),
            instructions: Vec::new(),
            ip: 0,
            stack: Vec::new(),
            object: object.clone(),
            block: Rc::new(RefCell::new(Object::new_with_prototype(object.clone()))),
            boolean: Rc::new(RefCell::new(Object::new_with_prototype(object.clone()))),
            iterator: Rc::new(RefCell::new(Object::new_with_prototype(object.clone()))),
            number: Rc::new(RefCell::new(Object::new_with_prototype(object.clone()))),
            string: Rc::new(RefCell::new(Object::new_with_prototype(object.clone()))),
            enable_tracing: false,
            line: usize::max_value(),
            col: usize::max_value(),
        };

        stdlib::setup(&mut vm);
        vm
    }
}

#[cfg(test)]
mod tests {
    use crate::vm;
    use std::rc::Rc;

    macro_rules! run {
        ($input:expr, $ip:expr, $type:ident, $value:expr) => {{
            let mut vm = vm::VirtualMachine::new();
            vm.ip = $ip;
            match vm.assemble($input) {
                Ok(()) => match vm.run() {
                    Ok(()) => {
                        assert_eq!(vm.ip, vm.instructions.len());
                        assert_eq!(vm.stack.len(), 1);
                        match vm.stack.pop() {
                            Some(vm::Value::$type(_, v)) => {
                                assert_eq!(v, $value);
                            }
                            _ => {
                                assert_eq!("Incorrect type.", "");
                            }
                        }
                    }
                    Err(e) => {
                        assert_eq!("Run failed.", e.err);
                    }
                },
                _ => {
                    assert_eq!("Assemble failed.", "");
                }
            }
        }};
    }

    macro_rules! runfails {
        ($input:expr, $err:tt) => {{
            let mut vm = vm::VirtualMachine::new();
            match vm.assemble($input) {
                Ok(()) => match vm.run() {
                    Ok(()) => {
                        assert!(false);
                    }
                    Err(e) => {
                        assert_eq!(e.err, $err);
                    }
                },
                Err(_) => {
                    assert!(false);
                }
            }
        }};
    }

    #[test]
    fn run() {
        let mut vm = vm::VirtualMachine::new();
        match vm.run() {
            Ok(()) => {
                assert_eq!(vm.stack.len(), 0);
                assert_eq!(vm.ip, 0);
            }
            _ => {
                assert!(false);
            }
        }

        run!(
            "
            const 1.0
            const 2.0
            add
        ",
            0,
            Number,
            3.0
        );

        run!(
            "
            const 1.0
            const 2.0
            div
        ",
            0,
            Number,
            0.5
        );

        run!(
            "
            const 1.0
            const 2.0
            mul
        ",
            0,
            Number,
            2.0
        );

        run!(
            "
            const 1.0
            const 2.0
            sub
        ",
            0,
            Number,
            -1.0
        );

        runfails!(
            "
            add
        ",
            "Stack underflow."
        );

        runfails!(
            "
            const 1.0
            add
        ",
            "Stack underflow."
        );

        runfails!(
            "
            const false
            const true
            add
        ",
            "Unsupported types for add."
        );

        runfails!(
            "
            const true
            const 1.0
            add
        ",
            "Unsupported types for add."
        );

        run!(
            "
            const 1.0
            const 2.0
            lt
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 2.0
            le
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            eq
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            ne
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const 1.0
            const 1.0
            gt
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const 1.0
            const 1.0
            ge
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            ge
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const true
            const false
            eq
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const -1.0
            const 2.0
            lt
            const false
            eq
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const 'hello
            const 'hello
            eq
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const '
            const '
            eq
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 'hello
            const 'world
            eq
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const 'hello
            const 2
            eq
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const nil
            const nil
            eq
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const true
            const nil
            eq
        ",
            0,
            Boolean,
            false
        );

        run!(
            "
            const true
            const nil
            ne
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 1.0
            const 1.0
            ne
        ",
            0,
            Boolean,
            false
        );

        runfails!(
            "
            const 1.0
            lookup
        ",
            "Lookup expects string."
        );

        runfails!(
            "
            lookup
        ",
            "Stack underflow."
        );

        runfails!(
            "
            const 'something
            lookup
        ",
            "Stack underflow."
        );

        let mut vm = vm::VirtualMachine::new();
        match vm.assemble(
            "
            const true
            dup
            const 'prototype
            lookup
            call
        ",
        ) {
            Ok(()) => match vm.run() {
                Ok(()) => {
                    assert_eq!(vm.stack.len(), 1);
                    assert_eq!(vm.ip, 5);
                    match vm.stack.pop() {
                        Some(vm::Value::Object(obj)) => {
                            assert!(Rc::ptr_eq(&obj, &vm.boolean));
                        }
                        _ => {
                            assert!(false);
                        }
                    }
                }
                _ => {
                    assert!(false);
                }
            },
            _ => {
                assert!(false);
            }
        }

        let mut vm = vm::VirtualMachine::new();
        match vm.assemble(
            "
            const true
            dup
            const 'prototype
            lookup
            call
            dup
            const 'prototype
            lookup
            call
        ",
        ) {
            Ok(()) => match vm.run() {
                Ok(()) => {
                    assert_eq!(vm.stack.len(), 1);
                    assert_eq!(vm.ip, 9);
                    match vm.stack.pop() {
                        Some(vm::Value::Object(obj)) => {
                            assert!(Rc::ptr_eq(&obj, &vm.object));
                        }
                        _ => {
                            assert!(false);
                        }
                    }
                }
                _ => {
                    assert!(false);
                }
            },
            _ => {
                assert!(false);
            }
        }

        runfails!(
            "
            const 2
            const 'unknown
            lookup
            call
        ",
            "Message not understood."
        );

        run!(
            "
            const 1.0
            const 1.0
            ne
            dup
            const 'not
            lookup
            call
        ",
            0,
            Boolean,
            true
        );

        run!(
            "
            const 'hello world
        ",
            0,
            String,
            "hello world"
        );

        run!(
            "
            const 1
            const 1
            pop
        ",
            0,
            Number,
            1.0
        );

        run!(
            "
            const 42
            ret
            block 0
            call
        ",
            2,
            Number,
            42.0
        );

        run!(
            "
            const 1
            const 1
            add
            ret
            block 0
            call
        ",
            4,
            Number,
            2.0
        );

        let mut vm = vm::VirtualMachine::new();
        match vm.assemble(
            "
            const true
            dup
            const 'prototype
            lookup
            srcpos 2 3
            call
        ",
        ) {
            Ok(()) => match vm.run() {
                Ok(()) => {
                    assert_eq!(vm.stack.len(), 1);
                    assert_eq!(vm.ip, 6);
                    match vm.stack.pop() {
                        Some(vm::Value::Object(obj)) => {
                            assert!(Rc::ptr_eq(&obj, &vm.boolean));
                        }
                        _ => {
                            assert!(false);
                        }
                    }
                    assert_eq!(vm.line, 2);
                    assert_eq!(vm.col, 3);
                }
                _ => {
                    assert!(false);
                }
            },
            _ => {
                assert!(false);
            }
        }
    }
}
