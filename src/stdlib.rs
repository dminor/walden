use crate::vm;
use std::cell::RefCell;
use std::rc::Rc;

macro_rules! err {
    ($vm: ident, $err:expr) => {{
        Err(vm::RuntimeError {
            err: $err.to_string(),
            line: $vm.line,
            col: $vm.col,
        })
    }};
}

fn boolean_and(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Boolean(proto, b)) => {
                vm.stack.push(vm::Value::Boolean(proto.clone(), a && b));
                Ok(())
            }
            None => err!(vm, "Stack underflow."),
            _ => err!(vm, "and: expects boolean."),
        },
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

// Interestingly, Smalltalk generates branch opcodes and inlines
// ifFalse:, ifTrue:ifFalse: and ifTrue: which means you can not
// define methods with these names for non-booleans.
fn boolean_iffalse(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Block(proto, params, locals, ip)) => {
                if !a {
                    vm.callstack.push((
                        vm::Value::Block(proto.clone(), params.to_vec(), locals.to_vec(), ip),
                        vm.stack.len(),
                        vm.ip + 1,
                    ));
                    vm.ip = ip;
                } else {
                    vm.stack.push(vm::Value::Nil(vm.object.clone()));
                }
                Ok(())
            }
            None => err!(vm, "Stack underflow."),
            _ => err!(vm, "ifFalse: expects block."),
        },
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn boolean_iftrue(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Block(proto, params, locals, ip)) => {
                if a {
                    vm.callstack.push((
                        vm::Value::Block(proto.clone(), params.to_vec(), locals.to_vec(), ip),
                        vm.stack.len(),
                        vm.ip + 1,
                    ));
                    vm.ip = ip;
                } else {
                    vm.stack.push(vm::Value::Nil(vm.object.clone()));
                }
                Ok(())
            }
            None => err!(vm, "Stack underflow."),
            _ => err!(vm, "ifTrue: expects block."),
        },
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn boolean_iftrue_iffalse(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Block(t_proto, t_params, t_locals, t_ip)) => match vm.stack.pop() {
                Some(vm::Value::Block(f_proto, f_params, f_locals, f_ip)) => {
                    if a {
                        vm.callstack.push((
                            vm::Value::Block(
                                t_proto.clone(),
                                t_params.to_vec(),
                                t_locals.to_vec(),
                                t_ip,
                            ),
                            vm.stack.len(),
                            vm.ip + 1,
                        ));
                        vm.ip = t_ip;
                    } else {
                        vm.callstack.push((
                            vm::Value::Block(
                                f_proto.clone(),
                                f_params.to_vec(),
                                f_locals.to_vec(),
                                f_ip,
                            ),
                            vm.stack.len(),
                            vm.ip + 1,
                        ));
                        vm.ip = f_ip;
                    }
                    Ok(())
                }
                None => err!(vm, "Stack underflow."),
                _ => err!(vm, "ifTrue: expects block."),
            },
            None => err!(vm, "Stack underflow."),
            _ => err!(vm, "ifFalse: expects block."),
        },
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn boolean_not(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(proto, b)) => {
            vm.stack.push(vm::Value::Boolean(proto.clone(), !b));
            Ok(())
        }
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn boolean_or(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Boolean(_, a)) => match vm.stack.pop() {
            Some(vm::Value::Boolean(proto, b)) => {
                vm.stack.push(vm::Value::Boolean(proto.clone(), a || b));
                Ok(())
            }
            None => err!(vm, "Stack underflow."),
            _ => err!(vm, "or: expects boolean."),
        },
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn object_clone(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Object(obj)) => {
            vm.stack.push(vm::Value::Object(Rc::new(RefCell::new(
                vm::Object::new_with_prototype(obj.clone()),
            ))));
            Ok(())
        }
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn object_override_with(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Object(obj)) => match vm.stack.pop() {
            Some(vm::Value::String(_, s)) => match vm.stack.pop() {
                Some(value) => {
                    obj.borrow_mut().override_with(s.to_string(), value);
                    vm.stack.push(vm::Value::Object(obj.clone()));
                    Ok(())
                }
                None => err!(vm, "Stack underflow."),
            },
            None => err!(vm, "Stack underflow."),
            _ => err!(vm, "override: expects string."),
        },
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn object_prototype(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(value) => match value {
            vm::Value::Block(proto, _, _, _)
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
        None => err!(vm, "Stack underflow."),
    }
}

fn object_set_to(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Block(proto, _, _, _)) => match vm.stack.pop() {
            Some(vm::Value::String(_, s)) => match vm.stack.pop() {
                Some(value) => {
                    proto.borrow_mut().set_to(s.to_string(), value);
                    if let Some(this) = proto.borrow().lookup("self".to_string()) {
                        vm.stack.push(this);
                    } else {
                        vm.stack.push(vm::Value::Object(vm.block.clone()));
                    }
                    Ok(())
                }
                None => err!(vm, "Stack underflow."),
            },
            None => err!(vm, "Stack underflow."),
            _ => err!(vm, "set: expects string."),
        },
        Some(vm::Value::Object(obj)) => match vm.stack.pop() {
            Some(vm::Value::String(_, s)) => match vm.stack.pop() {
                Some(value) => {
                    obj.borrow_mut().set_to(s.to_string(), value);
                    vm.stack.push(vm::Value::Object(obj.clone()));
                    Ok(())
                }
                None => err!(vm, "Stack underflow."),
            },
            None => err!(vm, "Stack underflow."),
            _ => err!(vm, "set: expects string."),
        },
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn block_disassemble(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Block(_, params, locals, ip)) => {
            let mut ip = ip;
            print!("@{} [", ip);
            for param in params {
                print!(" {}", param);
            }
            print!(" | ");
            for local in locals {
                print!(" {}", local);
            }
            println!(" ]");
            loop {
                println!("  {}: {}", ip, vm.instructions[ip]);
                match vm.instructions[ip] {
                    vm::Opcode::Ret => {
                        break;
                    }
                    _ => {
                        ip += 1;
                    }
                }
            }
            vm.stack.push(vm::Value::Nil(vm.object.clone()));
            Ok(())
        }
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn block_value(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(value) => match value {
            vm::Value::Block(proto, params, locals, ip) => {
                vm.callstack.push((
                    vm::Value::Block(proto.clone(), params.to_vec(), locals.to_vec(), ip),
                    vm.stack.len(),
                    vm.ip + 1,
                ));
                vm.ip = ip;
                Ok(())
            }
            _ => err!(vm, "Message not understood."),
        },
        None => err!(vm, "Stack underflow."),
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

pub fn setup(vm: &mut vm::VirtualMachine) {
    setobject!(vm.object, "clone", object_clone);
    setobject!(vm.object, "override:with:", object_override_with);
    setobject!(vm.object, "prototype", object_prototype);
    setobject!(vm.object, "set:to:", object_set_to);
    setobject!(vm.block, "disassemble", block_disassemble);
    setobject!(vm.block, "value", block_value);
    setobject!(vm.boolean, "and:", boolean_and);
    setobject!(vm.boolean, "ifFalse:", boolean_iffalse);
    setobject!(vm.boolean, "ifTrue:ifFalse:", boolean_iftrue_iffalse);
    setobject!(vm.boolean, "ifTrue:", boolean_iftrue);
    setobject!(vm.boolean, "not", boolean_not);
    setobject!(vm.boolean, "or:", boolean_or);

    setobject!(vm.block, "Object", vm::Value::Object(vm.object.clone()));
}
