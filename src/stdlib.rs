use crate::vm;
use std::cell::RefCell;
use std::rc::Rc;

fn boolean_and(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Boolean(proto, b)) => {
                vm.stack.push(vm::Value::Boolean(proto.clone(), a && b));
                Ok(())
            }
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
            _ => Err(vm::VMError {
                err: "and: expects boolean.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

// Interestingly, Smalltalk generates branch opcodes and inlines
// ifFalse:, ifTrue:ifFalse: and ifTrue: which means you can not
// define methods with these names for non-booleans.
fn boolean_iffalse(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Block(_, ip)) => {
                if !a {
                    vm.callstack.push((vm.stack.len(), vm.ip + 1));
                    vm.ip = ip;
                } else {
                    vm.stack.push(vm::Value::Nil(vm.object.clone()));
                }
                Ok(())
            }
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
            _ => Err(vm::VMError {
                err: "ifFalse: expects block.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn boolean_iftrue(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Block(_, ip)) => {
                if a {
                    vm.callstack.push((vm.stack.len(), vm.ip + 1));
                    vm.ip = ip;
                } else {
                    vm.stack.push(vm::Value::Nil(vm.object.clone()));
                }
                Ok(())
            }
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
            _ => Err(vm::VMError {
                err: "ifTrue: expects block.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn boolean_iftrue_iffalse(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Block(_, false_ip)) => match vm.stack.pop() {
                Some(vm::Value::Block(_, true_ip)) => {
                    if a {
                        vm.callstack.push((vm.stack.len(), vm.ip + 1));
                        vm.ip = true_ip;
                    } else {
                        vm.callstack.push((vm.stack.len(), vm.ip + 1));
                        vm.ip = false_ip;
                    }
                    Ok(())
                }
                None => Err(vm::VMError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                }),
                _ => Err(vm::VMError {
                    err: "ifTrue: expects block.".to_string(),
                    line: usize::max_value(),
                }),
            },
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
            _ => Err(vm::VMError {
                err: "ifFalse: expects block.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn boolean_not(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(proto, b)) => {
            vm.stack.push(vm::Value::Boolean(proto.clone(), !b));
            Ok(())
        }
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn boolean_or(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Boolean(proto, b)) => {
                vm.stack.push(vm::Value::Boolean(proto.clone(), a || b));
                Ok(())
            }
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
            _ => Err(vm::VMError {
                err: "or: expects boolean.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn object_clone(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Object(obj)) => {
            vm.stack.push(vm::Value::Object(Rc::new(RefCell::new(
                vm::Object::new_with_prototype(obj.clone()),
            ))));
            Ok(())
        }
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn object_prototype(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(value) => match value {
            vm::Value::Block(proto, _)
            | vm::Value::Boolean(proto, _)
            | vm::Value::Nil(proto)
            | vm::Value::Number(proto, _)
            | vm::Value::String(proto, _) => {
                vm.stack.push(vm::Value::Object(proto.clone()));
                Ok(())
            }
            vm::Value::Object(obj) => {
                let result;
                match &obj.borrow().prototype {
                    Some(proto) => result = vm::Value::Object(proto.clone()),
                    None => result = vm::Value::Nil(vm.object.clone()),
                }
                vm.stack.push(result);
                Ok(())
            }
            vm::Value::RustBlock(_, _) => unreachable!(),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn object_set_to(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(vm::Value::Block(proto, ip)) => match vm.stack.pop() {
            Some(value) => match vm.stack.pop() {
                Some(vm::Value::String(_, s)) => {
                    proto
                        .borrow_mut()
                        .members
                        .insert(s.to_string(), value.clone());
                    vm.stack.push(vm::Value::Block(proto.clone(), ip));
                    Ok(())
                }
                None => Err(vm::VMError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                }),
                _ => Err(vm::VMError {
                    err: "set: expects string.".to_string(),
                    line: usize::max_value(),
                }),
            },
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
        },
        Some(vm::Value::Object(obj)) => match vm.stack.pop() {
            Some(value) => match vm.stack.pop() {
                Some(vm::Value::String(_, s)) => {
                    obj.borrow_mut()
                        .members
                        .insert(s.to_string(), value.clone());
                    vm.stack.push(vm::Value::Object(obj.clone()));
                    Ok(())
                }
                None => Err(vm::VMError {
                    err: "Stack underflow.".to_string(),
                    line: usize::max_value(),
                }),
                _ => Err(vm::VMError {
                    err: "set: expects string.".to_string(),
                    line: usize::max_value(),
                }),
            },
            None => Err(vm::VMError {
                err: "Stack underflow.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
        _ => Err(vm::VMError {
            err: "Message not understood.".to_string(),
            line: usize::max_value(),
        }),
    }
}

fn object_value(vm: &mut vm::VirtualMachine) -> Result<(), vm::VMError> {
    match vm.stack.pop() {
        Some(value) => match value {
            vm::Value::Block(_, ip) => {
                vm.stack.push(value.clone());
                vm.callstack.push((vm.stack.len(), vm.ip + 1));
                vm.ip = ip;
                Ok(())
            }
            vm::Value::Boolean(proto, b) => {
                vm.stack.push(vm::Value::Boolean(proto.clone(), b));
                Ok(())
            }
            vm::Value::Nil(proto) => {
                vm.stack.push(vm::Value::Nil(proto.clone()));
                Ok(())
            }
            vm::Value::Number(proto, n) => {
                vm.stack.push(vm::Value::Number(proto.clone(), n));
                Ok(())
            }
            vm::Value::String(proto, s) => {
                vm.stack
                    .push(vm::Value::String(proto.clone(), s.to_string()));
                Ok(())
            }
            _ => Err(vm::VMError {
                err: "Message not understood.".to_string(),
                line: usize::max_value(),
            }),
        },
        None => Err(vm::VMError {
            err: "Stack underflow.".to_string(),
            line: usize::max_value(),
        }),
    }
}

macro_rules! setobject {
    ($target:expr, $name:expr, $fn:ident) => {{
        $target.borrow_mut().members.insert(
            $name.to_string(),
            vm::Value::RustBlock($name.to_string(), $fn),
        );
    }};
    ($target:expr, $name:expr, $obj:expr) => {{
        $target.borrow_mut().members.insert($name.to_string(), $obj);
    }};
}

pub fn create_standard_objects(vm: &mut vm::VirtualMachine) {
    setobject!(vm.object, "clone", object_clone);
    setobject!(vm.object, "prototype", object_prototype);
    setobject!(vm.object, "set:to:", object_set_to);
    setobject!(vm.object, "value", object_value);
    setobject!(vm.boolean, "and:", boolean_and);
    setobject!(vm.boolean, "ifFalse:", boolean_iffalse);
    setobject!(vm.boolean, "ifTrue:ifFalse:", boolean_iftrue_iffalse);
    setobject!(vm.boolean, "ifTrue:", boolean_iftrue);
    setobject!(vm.boolean, "not", boolean_not);
    setobject!(vm.boolean, "or:", boolean_or);

    setobject!(vm.global, "Object", vm::Value::Object(vm.object.clone()));
}
