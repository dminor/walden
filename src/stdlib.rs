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
                let proto = Rc::new(RefCell::new(vm::Object::new_with_prototype(proto.clone())));

                for param in params.iter() {
                    match vm.stack.pop() {
                        Some(value) => {
                            proto.borrow_mut().members.insert(param.to_string(), value);
                        }
                        None => {
                            return err!(vm, "Stack underflow");
                        }
                    }
                }
                for local in locals.iter() {
                    proto
                        .borrow_mut()
                        .members
                        .insert(local.to_string(), vm::Value::Nil(vm.object.clone()));
                }
                vm.callstack.push((
                    vm::Value::Block(proto.clone(), params.to_vec(), locals.to_vec(), ip),
                    vm.stack.len(),
                    vm.ip + 1,
                    false,
                ));
                vm.ip = ip;
                Ok(())
            }
            _ => err!(vm, "Message not understood."),
        },
        None => err!(vm, "Stack underflow."),
    }
}

fn block_whiletrue(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Block(cond_proto, cond_params, cond_locals, cond_ip)) => {
            match vm.stack.pop() {
                Some(vm::Value::Block(block_proto, block_params, block_locals, block_ip)) => {
                    let start_ip = vm.ip;
                    loop {
                        vm.callstack.push((
                            vm::Value::Block(
                                cond_proto.clone(),
                                cond_params.to_vec(),
                                cond_locals.to_vec(),
                                cond_ip,
                            ),
                            vm.stack.len(),
                            vm.ip + 1,
                            true,
                        ));
                        vm.ip = cond_ip;
                        match vm.run() {
                            Ok(()) => {}
                            Err(e) => {
                                return Err(e);
                            }
                        }
                        match vm.stack.pop() {
                            Some(vm::Value::Boolean(_, b)) => {
                                if !b {
                                    break;
                                }
                            }
                            _ => {
                                return err!(vm, "whileTrue: block should return boolean.");
                            }
                        }
                        vm.callstack.push((
                            vm::Value::Block(
                                block_proto.clone(),
                                block_params.to_vec(),
                                block_locals.to_vec(),
                                block_ip,
                            ),
                            vm.stack.len(),
                            vm.ip + 1,
                            true,
                        ));
                        vm.ip = block_ip;
                        match vm.run() {
                            Ok(()) => {}
                            Err(e) => {
                                return Err(e);
                            }
                        }
                        vm.stack.pop();
                    }
                    vm.stack.push(vm::Value::Nil(vm.object.clone()));
                    vm.ip = start_ip;
                    Ok(())
                }
                None => err!(vm, "Stack underflow."),
                _ => err!(vm, "whileTrue: expects block."),
            }
        }
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
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
                        false,
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
                        false,
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
                            false,
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
                            false,
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

fn iterator_next(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::Object(obj)) => {
            let mut obj = obj.borrow_mut();
            match obj.members.get(&"n".to_string()) {
                Some(v) => match v {
                    vm::Value::Number(_, n) => match obj.members.get(&"len".to_string()) {
                        Some(v) => match v {
                            vm::Value::Number(_, len) => {
                                if n < len {
                                    match obj.members.get(&n.to_string()) {
                                        Some(v) => {
                                            vm.stack.push(v.clone());
                                            let n = *n + 1.0;
                                            obj.override_with(
                                                "n".to_string(),
                                                vm::Value::Number(vm.number.clone(), n),
                                            );
                                            Ok(())
                                        }
                                        _ => err!(vm, "iterator: invalid index."),
                                    }
                                } else {
                                    vm.stack.push(vm::Value::Nil(vm.object.clone()));
                                    Ok(())
                                }
                            }
                            _ => err!(vm, "iterator: invalid length."),
                        },
                        None => err!(vm, "Stack underflow."),
                    },
                    _ => err!(vm, "iterator: invalid index."),
                },
                None => err!(vm, "Stack underflow."),
            }
        }
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
            | vm::Value::RustBlock(proto, _, _)
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

fn string_split(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::String(_, string)) => match vm.stack.pop() {
            Some(vm::Value::String(_, split)) => {
                let mut o = vm::Object::new_with_prototype(vm.iterator.clone());
                let mut len = 0.0;
                for s in string.split(&split) {
                    o.override_with(
                        len.to_string(),
                        vm::Value::String(vm.string.clone(), s.to_string()),
                    );
                    len += 1.0;
                }
                o.override_with("n".to_string(), vm::Value::Number(vm.number.clone(), 0.0));
                o.override_with("len".to_string(), vm::Value::Number(vm.number.clone(), len));
                vm.stack.push(vm::Value::Object(Rc::new(RefCell::new(o))));
                Ok(())
            }
            None => err!(vm, "Stack underflow."),
            _ => err!(vm, "startsWith: expects string."),
        },
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn string_startswith(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    match vm.stack.pop() {
        Some(vm::Value::String(_, string)) => match vm.stack.pop() {
            Some(vm::Value::String(_, start)) => {
                vm.stack.push(vm::Value::Boolean(
                    vm.boolean.clone(),
                    string.starts_with(&start),
                ));
                Ok(())
            }
            None => err!(vm, "Stack underflow."),
            _ => err!(vm, "startsWith: expects string."),
        },
        None => err!(vm, "Stack underflow."),
        _ => err!(vm, "Message not understood."),
    }
}

fn transcript_show(vm: &mut vm::VirtualMachine) -> Result<(), vm::RuntimeError> {
    vm.stack.pop();
    match vm.stack.pop() {
        Some(value) => {
            println!("{}", value);
            vm.stack.push(vm::Value::Nil(vm.object.clone()));
            Ok(())
        }
        None => err!(vm, "Stack underflow."),
    }
}

macro_rules! setobject {
    ($vm:ident, $target:expr, $name:expr, $fn:ident) => {{
        $target.borrow_mut().members.insert(
            $name.to_string(),
            vm::Value::RustBlock($vm.block.clone(), $name.to_string(), $fn),
        );
    }};
    ($target:expr, $name:expr, $obj:expr) => {{
        $target.borrow_mut().members.insert($name.to_string(), $obj);
    }};
}

pub fn setup(vm: &mut vm::VirtualMachine) {
    setobject!(vm, vm.block, "disassemble", block_disassemble);
    setobject!(vm, vm.block, "value", block_value);
    setobject!(vm, vm.block, "value:", block_value);
    setobject!(vm, vm.block, "value:value:", block_value);
    setobject!(vm, vm.block, "whileTrue:", block_whiletrue);
    setobject!(vm, vm.boolean, "and:", boolean_and);
    setobject!(vm, vm.boolean, "ifFalse:", boolean_iffalse);
    setobject!(vm, vm.boolean, "ifTrue:ifFalse:", boolean_iftrue_iffalse);
    setobject!(vm, vm.boolean, "ifTrue:", boolean_iftrue);
    setobject!(vm, vm.boolean, "not", boolean_not);
    setobject!(vm, vm.boolean, "or:", boolean_or);
    setobject!(vm, vm.iterator, "next", iterator_next);
    setobject!(vm, vm.object, "clone", object_clone);
    setobject!(vm, vm.object, "override:with:", object_override_with);
    setobject!(vm, vm.object, "prototype", object_prototype);
    setobject!(vm, vm.object, "set:to:", object_set_to);
    setobject!(vm, vm.string, "split:", string_split);
    setobject!(vm, vm.string, "startsWith:", string_startswith);

    setobject!(vm.block, "Object", vm::Value::Object(vm.object.clone()));

    let transcript = Rc::new(RefCell::new(vm::Object::new_with_prototype(
        vm.object.clone(),
    )));
    setobject!(vm, transcript, "show:", transcript_show);

    setobject!(
        vm.block,
        "Transcript",
        vm::Value::Object(transcript.clone())
    );
}
