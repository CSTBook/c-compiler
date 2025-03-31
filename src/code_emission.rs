use std::fs::File;
use std::io::Write;

use crate::assembler::{
    AsmFunction, AsmProgram, BinaryOp, CondCode, Instruction, Operand, UnaryOp,
};

pub fn code_emission(program: AsmProgram, filename: String) -> String {
    let mut out_file = File::create(filename.clone() + ".s").unwrap();
    write!(out_file, "{}", code_gen(program)).unwrap();
    filename + ".s"
}

fn code_gen(program: AsmProgram) -> String {
    code_gen_fn(program.function) // + "\n.section .note.GNU-stack,\"\",@progbits\n"
}

fn code_gen_fn(function: AsmFunction) -> String {
    let mut output = format!(
        "\t.globl _{}\n_{}:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp",
        function.name, function.name
    );
    for instruction in function.instructions {
        output += &format!("\n{}", code_gen_instr(instruction));
    }
    output
}

fn code_gen_instr(instruction: Instruction) -> String {
    match instruction {
        Instruction::Mov(src, dst) => format!(
            "\tmovl\t{}, {}",
            code_gen_op(src, false),
            code_gen_op(dst, false)
        ),
        Instruction::Unary(unop, operand) => {
            format!("\t{}\t{}", code_gen_unop(unop), code_gen_op(operand, false))
        }
        Instruction::AllocateStack(bytes) => format!("\tsubq\t${}, %rsp", bytes),
        Instruction::Ret => String::from("\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"),
        Instruction::Binary(binop, src, dst) => format!(
            "\t{}\t{}, {}",
            code_gen_binop(binop.clone()),
            code_gen_op(
                src,
                matches!(
                    binop,
                    BinaryOp::BitwiseLeftShift | BinaryOp::BitwiseRightShift
                )
            ),
            code_gen_op(dst, false)
        ),
        Instruction::Idiv(operand) => format!("\tidivl\t{}", code_gen_op(operand, false)),
        Instruction::Cdq => String::from("\tcdq"),
        Instruction::Cmp(op1, op2) => format!(
            "\tcmpl\t{}, {}",
            code_gen_op(op1, false),
            code_gen_op(op2, false)
        ),
        Instruction::Jmp(label) => format!("\tjmp L{label}"),
        Instruction::JmpCC(cc, label) => format!("\tj{}\tL{}", code_gen_cc(cc), label),
        Instruction::SetCC(cc, op) => {
            format!("\tset{}\t{}", code_gen_cc(cc), code_gen_op(op, true))
        }
        Instruction::Label(label) => format!("\tL{label}:"),
    }
}

fn code_gen_op(operand: Operand, one_byte: bool) -> String {
    match operand {
        Operand::Imm(val) => format!("${}", val),
        Operand::Reg(register) => match register {
            crate::assembler::Register::AX => {
                if one_byte {
                    String::from("%al")
                } else {
                    String::from("%eax")
                }
            }
            crate::assembler::Register::R10 => {
                if one_byte {
                    String::from("%r10b")
                } else {
                    String::from("%r10d")
                }
            }
            crate::assembler::Register::DX => {
                if one_byte {
                    String::from("%dl")
                } else {
                    String::from("%edx")
                }
            }
            crate::assembler::Register::R11 => {
                if one_byte {
                    String::from("%r11b")
                } else {
                    String::from("%r11d")
                }
            }
            crate::assembler::Register::CX => {
                if one_byte {
                    String::from("%cl")
                } else {
                    String::from("%ecx")
                }
            }
        },
        Operand::Stack(bytes) => format!("{}(%rbp)", bytes),
        Operand::Pseudo(_) => unreachable!("Pseudoregister not removed"),
    }
}

fn code_gen_unop(unop: UnaryOp) -> String {
    match unop {
        UnaryOp::Neg => String::from("negl"),
        UnaryOp::Not => String::from("notl"),
    }
}

fn code_gen_binop(binop: BinaryOp) -> String {
    match binop {
        BinaryOp::Add => String::from("addl"),
        BinaryOp::Sub => String::from("subl"),
        BinaryOp::Mult => String::from("imull"),
        BinaryOp::BitwiseAnd => String::from("andl"),
        BinaryOp::BitwiseOr => String::from("orl "),
        BinaryOp::BitwiseXor => String::from("xorl"),
        BinaryOp::BitwiseLeftShift => String::from("sall"),
        BinaryOp::BitwiseRightShift => String::from("sarl"),
    }
}

fn code_gen_cc(cc: CondCode) -> String {
    match cc {
        CondCode::E => String::from("e"),
        CondCode::NE => String::from("ne"),
        CondCode::G => String::from("g"),
        CondCode::GE => String::from("ge"),
        CondCode::L => String::from("l"),
        CondCode::LE => String::from("le"),
    }
}
