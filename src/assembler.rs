use crate::parser::AstNode;

pub enum AsmNode {
    Program(Box<AsmNode>),
    Function(String, Vec<AsmNode>),
    Mov(Box<AsmNode>, Box<AsmNode>),
    Ret,
    Imm(i32),
    Register
}

pub fn assembler(program_ast: AstNode) -> AsmNode {
    let program = parse_program(program_ast);
    program
}

fn parse_program(program_ast: AstNode) -> AsmNode {
    match program_ast {
        AstNode::Program(func) => AsmNode::Program(Box::new(parse_function(*func))),
        _ => AsmNode::Ret //will never happen
    }
}

fn parse_function(function_ast: AstNode) -> AsmNode {
    match function_ast {
        AstNode::Function(name, body) => AsmNode::Function(name, parse_instruction(*body)),
        _ => AsmNode::Ret //will never happpen
    }
}

fn parse_instruction(body_ast: AstNode) -> Vec<AsmNode> {
    let mut instructions: Vec<AsmNode> = Vec::new();

    match body_ast {
        AstNode::Return(exp) => {
            instructions.push(AsmNode::Mov(Box::new(parse_expression(*exp)),Box::new(AsmNode::Register)));
            instructions.push(AsmNode::Ret);
        },
        _ => () //will never happen
    }

    instructions
}

fn parse_expression(exp_ast: AstNode) -> AsmNode {
    match exp_ast {
        AstNode::Constant(val) => AsmNode::Imm(val),
        _ => AsmNode::Ret //will never happen
    }
}

#[allow(dead_code)]
pub fn pretty_printer(node: &AsmNode, indent_level: usize) -> String {
    match node {
        AsmNode::Program(func) => {
            format!("{}Program(\n{}{}\n{})",tabs(indent_level), tabs(indent_level), pretty_printer(func,indent_level+1), tabs(indent_level))
        },
        AsmNode::Function(name, instructions) => {
            let mut output = format!("{}Function(\n{}name=\"{}\"\n{}body=(",tabs(indent_level),tabs(indent_level+1),name,tabs(indent_level+1));
            for instruction in instructions {
                output += &format!("\n{}",pretty_printer(instruction, indent_level+2));
            }
            output+=&format!("\n{})\n{})",tabs(indent_level+1),tabs(indent_level));
            output
        },
        AsmNode::Mov(dest, src) => {
            format!("{}Mov({},{})",tabs(indent_level),pretty_printer(dest, indent_level+2),pretty_printer(src,indent_level+2))
        },
        AsmNode::Imm(val) => {
            format!("Imm({})", val)
        },
        AsmNode::Register => {
            "Register".to_string()
        },
        AsmNode::Ret => {
            format!("{}Ret",tabs(indent_level))
        }
    }
}

#[allow(dead_code)]
fn tabs(indent_level: usize) -> String {
    "  ".repeat(indent_level)
}