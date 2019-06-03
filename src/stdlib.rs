use crate::vm;
use std::rc::Rc;

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

pub fn create_standard_objects(vm: &mut vm::VirtualMachine) {
    match Rc::get_mut(&mut vm.boolean) {
        Some(obj) => {
            obj.members
                .insert("not".to_string(), vm::Value::RustBlock(boolean_not));
        }
        None => {}
    };
}
