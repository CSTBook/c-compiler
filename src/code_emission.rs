use std::fs::File;
use std::io::Write;

use crate::assembler::AsmNode;

pub fn code_emission(program: AsmNode, filename: String) -> String {
    let mut out_file = File::create(filename.clone() + ".s").unwrap();
    write!(out_file, "{}", code_gen(program)).unwrap();
    filename + ".s"
}

fn code_gen(program: AsmNode) -> String {
    match program {
        AsmNode::Program(func) => code_gen(*func),
        AsmNode::Function(name, instructions) => {
            let mut output = format!("\t.globl _{}\n_{}:", name, name);
            for instruction in instructions {
                // output += &format!("\n{}", code_gen(instruction));
            }
            output
        }
        // AsmNode::Mov(src, dest) => format!("\tmovl {},{}", code_gen(*src), code_gen(*dest)),
        // AsmNode::Ret => String::from("\tret"),
        // AsmNode::Imm(val) => format!("\t${}", val),
        // AsmNode::Register => String::from("%eax"),
    }
}
