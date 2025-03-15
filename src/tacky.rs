use std::sync::Mutex;

use crate::parser::{self, AstNode, Unary};

pub enum TackyNode {
    Program(Box<TackyNode>),
    Function(String, Vec<TackyInstruction>),
}

pub enum TackyInstruction {
    Return(TackyValue),
    Unary(Unary, TackyValue, TackyValue), //Unary(unop, src, dest) where dest must be a temp var
}

pub enum TackyValue {
    Constant(i32),
    Var(String),
}

static TEMP_COUNTER: Mutex<i32> = Mutex::new(0);

pub fn emit_tacky(program_ast: AstNode) -> TackyNode {
    parse_program(program_ast)
}

fn parse_program(program_ast: AstNode) -> TackyNode {
    match program_ast {
        AstNode::Program(func) => TackyNode::Program(Box::new(parse_function(*func))),
        _ => panic!("Tacky Program Parser Error"),
    }
}

fn parse_function(function_ast: AstNode) -> TackyNode {
    match function_ast {
        AstNode::Function(name, body) => TackyNode::Function(name, parse_instructions(body)),
        _ => panic!("Tacky Function Parser Error"),
    }
}

fn parse_instructions(instructions_ast: parser::Statement) -> Vec<TackyInstruction> {
    let mut instructions: Vec<TackyInstruction> = Vec::new();
    match instructions_ast {
        parser::Statement::Return(exp) => {
            let temp = parse_exp(exp, &mut instructions);
            instructions.push(TackyInstruction::Return(temp));
        }
    }
    instructions
}

fn parse_exp(
    expression: parser::Expression,
    instructions: &mut Vec<TackyInstruction>,
) -> TackyValue {
    match expression {
        parser::Expression::Constant(val) => TackyValue::Constant(val),
        parser::Expression::Unary(unop, exp) => {
            let src = parse_exp(*exp, instructions);
            let name = make_temporary_variable();
            let dest = TackyValue::Var(name.clone());
            instructions.push(TackyInstruction::Unary(
                unop,
                src,
                TackyValue::Var(name.clone()),
            ));

            dest
        }
    }
}

fn make_temporary_variable() -> String {
    let name = format!("tmp.{}", TEMP_COUNTER.lock().unwrap());
    *(TEMP_COUNTER.lock().unwrap()) += 1;
    name
}

//TODO: print Var such that it shows the storing
#[allow(dead_code)]
pub fn pretty_printer(node: &TackyNode, indent_level: usize) -> String {
    match node {
        TackyNode::Program(func) => {
            format!(
                "{}Program(\n{}{}\n{})",
                tabs(indent_level),
                tabs(indent_level),
                pretty_printer(func, indent_level + 1),
                tabs(indent_level)
            )
        }
        TackyNode::Function(name, instructions) => {
            let mut output = format!(
                "{}Function(\n{}name=\"{}\"\n{}body=(",
                tabs(indent_level),
                tabs(indent_level + 1),
                name,
                tabs(indent_level + 1)
            );

            for instruction in instructions {
                output += &format!("\n{}", pretty_printer_instr(instruction, indent_level + 2));
            }
            output += &format!("\n{})\n{})", tabs(indent_level + 1), tabs(indent_level));

            output
        }
    }
}

fn pretty_printer_instr(instruction: &TackyInstruction, indent_level: usize) -> String {
    match instruction {
        TackyInstruction::Return(exp) => {
            format!(
                "{}Return(\n{}\n{})",
                tabs(indent_level),
                pretty_printer_val(exp, indent_level + 1),
                tabs(indent_level)
            )
        }
        TackyInstruction::Unary(unary, src, dest) => {
            let mut output = format!("{}Unary(", tabs(indent_level));
            output += match unary {
                Unary::Complement => "Complement",
                Unary::Negate => "Negate",
            };
            output += &format!(
                ")(\n{}\n{}) in {}",
                pretty_printer_val(src, indent_level + 1),
                tabs(indent_level),
                pretty_printer_val(dest, 0)
            );
            output
        }
    }
}

fn pretty_printer_val(value: &TackyValue, indent_level: usize) -> String {
    match value {
        TackyValue::Constant(val) => {
            format!("{}Constant({})", tabs(indent_level), val)
        }
        TackyValue::Var(name) => {
            format!("{}Var({})", tabs(indent_level), name)
        }
    }
}

#[allow(dead_code)]
fn tabs(indent_level: usize) -> String {
    "  ".repeat(indent_level)
}
