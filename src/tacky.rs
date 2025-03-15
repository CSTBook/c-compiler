use std::sync::Mutex;

use crate::parser::{AstNode,Unary};

pub enum TackyNode {
    Program(Box<TackyNode>),
    Function(String, Vec<TackyNode>),
    Return(Box<TackyNode>),
    Unary(Unary, Box<TackyNode>, Box<TackyNode>), //Unary(unop, src, dest) where dest must be a temp var
    Constant(i32),
    Var(String)
}

static TEMP_COUNTER: Mutex<i32> = Mutex::new(0);

pub fn emit_tacky(program_ast: AstNode) -> TackyNode {
    let program = parse_program(program_ast);

    program
}

fn parse_program(program_ast: AstNode) -> TackyNode {
    match program_ast {
        AstNode::Program(func) => {
            TackyNode::Program(Box::new(parse_function(*func)))
        },
        _ => TackyNode::Constant(0) //will never happen
    }
}

fn parse_function(function_ast: AstNode) -> TackyNode {
    match function_ast {
        AstNode::Function(name, body) => {
            TackyNode::Function(name, parse_instruction( *body))
        },
        _ => TackyNode::Constant(0) //will never happen
    }
}

fn parse_instruction(instruction_ast: AstNode) -> Vec<TackyNode> {
    let mut instructions: Vec<TackyNode> = Vec::new();
    match instruction_ast {
        AstNode::Return(exp) => {
            instructions.push(parse_exp(*exp));
        },
        _ => (),
    }
    instructions
}

fn parse_exp(val_ast: AstNode) -> TackyNode {
    TackyNode::Return(Box::new(match val_ast {
        AstNode::Constant(val) => TackyNode::Constant(val),
        AstNode::Unary(unop, exp) => {
            let src = parse_exp(*exp);
            let name = make_temporary_variable();
            let dest = TackyNode::Var(name);
            TackyNode::Unary(unop, Box::new(src), Box::new(dest))
        },
        _ => TackyNode::Constant(0) // will never happen
    }))
}

fn make_temporary_variable() -> String {
    let name = format!("tmp.{}",TEMP_COUNTER.lock().unwrap());
    *(TEMP_COUNTER.lock().unwrap())+=1;
    name
}

//TODO: print Var such that it shows the storing
#[allow(dead_code)]
pub fn pretty_printer(node: &TackyNode, indent_level: usize) -> String {
    match node {
        TackyNode::Program(func) => {
            format!("{}Program(\n{}{}\n{})",tabs(indent_level), tabs(indent_level), pretty_printer(func,indent_level+1), tabs(indent_level))
        },
        TackyNode::Function(name, instructions) => {
            let mut output = format!("{}Function(\n{}name=\"{}\"\n{}body=(",tabs(indent_level),tabs(indent_level+1),name,tabs(indent_level+1));

            for instruction in instructions {
                output += &format!("\n{}",pretty_printer(instruction, indent_level+2));
            }
            output+=&format!("\n{})\n{})",tabs(indent_level+1),tabs(indent_level));

            output
        },
        TackyNode::Return(exp) => {
            format!("{}Return(\n{}\n{})", tabs(indent_level), pretty_printer(exp, indent_level+1),tabs(indent_level))
        },
        TackyNode::Unary(unary, src,dest) => {
            let mut output = format!("{}Unary(",tabs(indent_level));
            output += match unary {
                Unary::Complement => "Complement",
                Unary::Negate => "Negate",
            };
            output += &format!(")\n{}\n{}",pretty_printer(src, indent_level),pretty_printer(dest, indent_level));
            output
        },
        TackyNode::Constant(val) => {
            format!("{}Constant({})",tabs(indent_level),val)
        },
        TackyNode::Var(name) => {
            format!("{}Var({})",tabs(indent_level),name)
        }
    }
}

#[allow(dead_code)]
fn tabs(indent_level: usize) -> String {
    "  ".repeat(indent_level)
}