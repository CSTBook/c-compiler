pub use crate::assembler::*;
pub use crate::parser::*;
pub use crate::tacky::*;
pub mod parser {
    use super::*;
    pub fn pretty_printer(program: &Program, indent_level: usize) -> String {
        format!(
            "{}Program(\n{}{}\n{})",
            tabs(indent_level),
            tabs(indent_level),
            pretty_printer_fn(&program.function, indent_level + 1),
            tabs(indent_level)
        )
    }

    pub fn pretty_printer_fn(func: &Function, indent_level: usize) -> String {
        let mut output = format!(
            "{}Function(\n{}name=\"{}\"\n{}body=",
            tabs(indent_level),
            tabs(indent_level + 1),
            func.name,
            tabs(indent_level + 1)
        );
        for instruction in func.body.clone() {
            output +=
                format!("\n{}", pretty_printer_block(&instruction, indent_level + 1)).as_str();
        }

        output += format!("\n{})", tabs(indent_level)).as_str();
        output
    }

    pub fn pretty_printer_block(block: &BlockItem, indent_level: usize) -> String {
        match block {
            BlockItem::Statement(statement) => {
                format!(
                    "{}{}\n{})",
                    tabs(indent_level),
                    pretty_printer_statement(statement, indent_level + 1),
                    tabs(indent_level)
                )
            }
            BlockItem::Declaration(declaration) => {
                format!(
                    "{}{}",
                    tabs(indent_level),
                    pretty_printer_dec(declaration, indent_level + 1)
                )
            }
        }
    }

    fn pretty_printer_dec(declaration: &Declaration, indent_level: usize) -> String {
        if let Some(exp) = &declaration.init {
            return format!(
                "Declaration({}=\n{}\n{})",
                declaration.name,
                pretty_printer_expr(exp, indent_level + 1),
                tabs(indent_level - 1)
            );
        }
        format!("Declaration({})", declaration.name) //uninitialized variable
    }

    fn pretty_printer_statement(statement: &Statement, indent_level: usize) -> String {
        match statement {
            Statement::Return(expression) => format!(
                "Return(\n{}",
                pretty_printer_expr(expression, indent_level + 1)
            ),
            Statement::Expression(expression) => format!(
                "Expression(\n{}",
                pretty_printer_expr(expression, indent_level + 1)
            ),
            Statement::Null => String::from("Null"),
            Statement::If(condition, then, else_statement) => {
                let mut output = format!(
                    "{}If (\n{}condition=\n{},\n{}then={},",
                    tabs(indent_level),
                    tabs(indent_level + 1),
                    pretty_printer_expr(condition, indent_level + 2),
                    tabs(indent_level + 1),
                    pretty_printer_statement(then, indent_level + 2)
                );
                if let Some(stmt) = else_statement {
                    output += format!(
                        "\n{}else={},",
                        tabs(indent_level + 1),
                        pretty_printer_statement(stmt, indent_level + 2)
                    )
                    .as_str();
                }

                output += &format!("\n{})", tabs(indent_level));

                output
            }
        }
    }

    fn pretty_printer_expr(expression: &Expression, indent_level: usize) -> String {
        match expression {
            Expression::Constant(val) => {
                format!("{}Constant({})", tabs(indent_level), val)
            }
            Expression::Unary(unary, ast_node) => {
                let mut output = format!("{}Unary(", tabs(indent_level));
                output += match unary {
                    UnaryParser::Complement => "Complement",
                    UnaryParser::Negate => "Negate",
                    UnaryParser::Not => "Not",
                };
                output += &format!(")\n{}", pretty_printer_expr(ast_node, indent_level + 1));
                output
            }
            Expression::Binary(binop, left, right) => {
                format!(
                    "{}Binary({}) (\n{}\n{}\n{})",
                    tabs(indent_level),
                    pretty_printer_binop(binop, 0),
                    pretty_printer_expr(left, indent_level + 1),
                    pretty_printer_expr(right, indent_level + 1),
                    tabs(indent_level)
                )
            }
            Expression::Var(name) => format!("{}Var({})", tabs(indent_level), name),
            Expression::Assignment(lvalue, rvalue) => {
                format!(
                    "{}Assignment(\n{}\n{}\n{})",
                    tabs(indent_level),
                    pretty_printer_expr(lvalue, indent_level + 1),
                    pretty_printer_expr(rvalue, indent_level + 1),
                    tabs(indent_level)
                )
            }
            Expression::Conditional(condition, then, right) => format!(
                "If\n{}condition={},\n{}then={},\n{}else={},",
                tabs(indent_level + 1),
                pretty_printer_expr(condition, indent_level + 2),
                tabs(indent_level + 1),
                pretty_printer_expr(then, indent_level + 2),
                tabs(indent_level + 1),
                pretty_printer_expr(right, indent_level + 2)
            ),
        }
    }

    fn pretty_printer_binop(binop: &BinaryParser, indent_level: usize) -> String {
        match binop {
            BinaryParser::Add => format!("{}Add", tabs(indent_level)),
            BinaryParser::Subtract => format!("{}Subtract", tabs(indent_level)),
            BinaryParser::Multiply => format!("{}Multiply", tabs(indent_level)),
            BinaryParser::Divide => format!("{}Divide", tabs(indent_level)),
            BinaryParser::Remainder => format!("{}Remainder", tabs(indent_level)),
            BinaryParser::BitwiseAnd => format!("{}BitwiseAnd", tabs(indent_level)),
            BinaryParser::BitwiseOr => format!("{}BitwiseOr", tabs(indent_level)),
            BinaryParser::BitwiseXor => format!("{}BitwiseXor", tabs(indent_level)),
            BinaryParser::BitwiseLeftShift => format!("{}BitwiseRightShift", tabs(indent_level)),
            BinaryParser::BitwiseRightShift => format!("{}BitwiseLeftShift", tabs(indent_level)),
            BinaryParser::And => format!("{}And", tabs(indent_level)),
            BinaryParser::Or => format!("{}Or", tabs(indent_level)),
            BinaryParser::Equal => format!("{}Equal", tabs(indent_level)),
            BinaryParser::NotEqual => format!("{}NotEqual", tabs(indent_level)),
            BinaryParser::LessThan => format!("{}LessThan", tabs(indent_level)),
            BinaryParser::LessOrEqual => format!("{}LessOrEqual", tabs(indent_level)),
            BinaryParser::GreaterThan => format!("{}GreaterThan", tabs(indent_level)),
            BinaryParser::GreaterOrEqual => format!("{}GreaterOrEqual", tabs(indent_level)),
            BinaryParser::PostfixIncrement => format!("{}PostfixIncrement", tabs(indent_level)),
            BinaryParser::PostfixDecrement => format!("{}PostfixDecrement", tabs(indent_level)),
        }
    }
}
pub mod tacky {
    use super::*;
    pub fn pretty_printer(node: &TackyProgram, indent_level: usize) -> String {
        format!(
            "{}Program(\n{}{}\n{})",
            tabs(indent_level),
            tabs(indent_level),
            pretty_printer_fn(&node.function, indent_level + 1),
            tabs(indent_level)
        )
    }

    fn pretty_printer_fn(func: &TackyFunction, indent_level: usize) -> String {
        let name = func.name.clone();

        let mut output = format!(
            "{}Function(\n{}name=\"{}\"\n{}body=(",
            tabs(indent_level),
            tabs(indent_level + 1),
            name,
            tabs(indent_level + 1)
        );

        for instruction in &func.body {
            output += &format!("\n{}", pretty_printer_instr(instruction, indent_level + 2));
        }
        output += &format!("\n{})\n{})", tabs(indent_level + 1), tabs(indent_level));

        output
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
                    UnaryParser::Complement => "Complement",
                    UnaryParser::Negate => "Negate",
                    UnaryParser::Not => "Not",
                };
                output += &format!(
                    ")(\n{}\n{}) in {}",
                    pretty_printer_val(src, indent_level + 1),
                    tabs(indent_level),
                    pretty_printer_val(dest, 0)
                );
                output
            }
            TackyInstruction::Binary(binop, src1, src2, dst) => {
                let mut output = format!("{}Binary(", tabs(indent_level));
                output += match binop {
                    BinaryParser::Add => "Add",
                    BinaryParser::Subtract => "Subtract",
                    BinaryParser::Multiply => "Multiply",
                    BinaryParser::Divide => "Divide",
                    BinaryParser::Remainder => "Remainder",
                    BinaryParser::BitwiseAnd => "BitwiseAnd",
                    BinaryParser::BitwiseOr => "BitwiseOr",
                    BinaryParser::BitwiseXor => "BitwiseXor",
                    BinaryParser::BitwiseLeftShift => "BitwiseLeftShift",
                    BinaryParser::BitwiseRightShift => "BitwiseRightShift",
                    BinaryParser::And => "And",
                    BinaryParser::Or => "Or",
                    BinaryParser::Equal => "Equal",
                    BinaryParser::NotEqual => "NotEqual",
                    BinaryParser::LessThan => "LessThan",
                    BinaryParser::LessOrEqual => "LessOrEqual",
                    BinaryParser::GreaterThan => "GreaterThan",
                    BinaryParser::GreaterOrEqual => "GreaterOrEqual",
                    BinaryParser::PostfixIncrement => "PostfixDecrement",
                    BinaryParser::PostfixDecrement => "PostfixIncrement",
                };
                output += &format!(
                    ")(\n{}\n{}\n{}) in {}",
                    pretty_printer_val(src1, indent_level + 1),
                    pretty_printer_val(src2, indent_level + 1),
                    tabs(indent_level),
                    pretty_printer_val(dst, 0)
                );
                output
            }
            TackyInstruction::Copy(src, dst) => {
                format!(
                    "{}Copy(\n{}\n{}\n{})",
                    tabs(indent_level),
                    pretty_printer_val(src, indent_level + 1),
                    pretty_printer_val(dst, indent_level + 1),
                    tabs(indent_level)
                )
            }
            TackyInstruction::Jump(target) => format!("{}Jump({})", tabs(indent_level), target),
            TackyInstruction::JumpIfZero(cond, target) => format!(
                "{}JumpIfZero({}) to Label({})",
                tabs(indent_level),
                pretty_printer_val(cond, 0),
                target
            ),
            TackyInstruction::JumpIfNotZero(cond, target) => format!(
                "{}JumpIfNotZero({}) to Label({})",
                tabs(indent_level),
                pretty_printer_val(cond, 0),
                target
            ),
            TackyInstruction::Label(label) => format!("{}Label({})", tabs(indent_level), label),
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
}
pub mod assembler {
    use super::*;
    pub fn pretty_printer(program: &AsmProgram, indent_level: usize) -> String {
        format!(
            "{}Program(\n{}{}\n{})",
            tabs(indent_level),
            tabs(indent_level),
            pretty_printer_fn(&program.function, indent_level + 1),
            tabs(indent_level)
        )
    }

    fn pretty_printer_fn(function: &AsmFunction, indent_level: usize) -> String {
        let mut output = format!(
            "{}Function(\n{}name=\"{}\"\n{}body=(",
            tabs(indent_level),
            tabs(indent_level + 1),
            function.name,
            tabs(indent_level + 1)
        );
        for instruction in function.instructions.clone() {
            output += &format!("\n{}", pretty_printer_instr(&instruction, indent_level + 2));
        }
        output += &format!("\n{})\n{})", tabs(indent_level + 1), tabs(indent_level));
        output
    }

    #[allow(dead_code)]
    fn pretty_printer_instr(instruction: &Instruction, indent_level: usize) -> String {
        match instruction {
            Instruction::Mov(src, dest) => {
                format!(
                    "{}Mov({},{})",
                    tabs(indent_level),
                    pretty_printer_opr(src),
                    pretty_printer_opr(dest)
                )
            }
            Instruction::Unary(unop, operand) => format!(
                "{}{}({})",
                tabs(indent_level),
                pretty_printer_unop(unop),
                pretty_printer_opr(operand)
            ),
            Instruction::AllocateStack(val) => {
                format!("{}AllocateStack({})", tabs(indent_level), *val)
            }
            Instruction::Ret => format!("{}Ret", tabs(indent_level)),
            Instruction::Binary(binop, src, dst) => format!(
                "{}{}({},{})",
                tabs(indent_level),
                pretty_printer_binop(binop),
                pretty_printer_opr(src),
                pretty_printer_opr(dst)
            ),
            Instruction::Idiv(operand) => format!(
                "{}Idiv({})",
                tabs(indent_level),
                pretty_printer_opr(operand)
            ),
            Instruction::Cdq => format!("{}Cdq", tabs(indent_level)),
            Instruction::Cmp(op1, op2) => format!(
                "{}Cmp({},{})",
                tabs(indent_level),
                pretty_printer_opr(op1),
                pretty_printer_opr(op2)
            ),
            Instruction::Jmp(target) => format!("{}Jmp({})", tabs(indent_level), target),
            Instruction::JmpCC(cond_code, target) => format!(
                "{}JmpCC({},{})",
                tabs(indent_level),
                pretty_printer_cc(cond_code),
                target
            ),
            Instruction::SetCC(cond_code, operand) => format!(
                "{}SetCC({},{})",
                tabs(indent_level),
                pretty_printer_cc(cond_code),
                pretty_printer_opr(operand)
            ),
            Instruction::Label(label) => format!("{}Label({})", tabs(indent_level), label),
        }
    }

    fn pretty_printer_unop(unop: &UnaryOp) -> String {
        match unop {
            UnaryOp::Neg => String::from("Neg"),
            UnaryOp::Not => String::from("Not"),
        }
    }

    fn pretty_printer_binop(binop: &BinaryOp) -> String {
        match binop {
            BinaryOp::Add => String::from("Add"),
            BinaryOp::Sub => String::from("Sub"),
            BinaryOp::Mult => String::from("Mult"),
            BinaryOp::BitwiseAnd => String::from("BitwiseAnd"),
            BinaryOp::BitwiseOr => String::from("BitwiseOr"),
            BinaryOp::BitwiseXor => String::from("BitwiseXor"),
            BinaryOp::BitwiseLeftShift => String::from("BitwiseLeftShift"),
            BinaryOp::BitwiseRightShift => String::from("BitwiseRightShift"),
        }
    }

    fn pretty_printer_opr(operand: &Operand) -> String {
        match operand {
            Operand::Imm(val) => format!("Imm({})", *val),
            Operand::Reg(register) => format!("Reg({})", pretty_printer_reg(register)),
            Operand::Pseudo(name) => format!("Var({})", name),
            Operand::Stack(val) => format!("Stack({})", *val),
        }
    }

    fn pretty_printer_reg(register: &Register) -> String {
        match register {
            Register::AX => String::from("AX"),
            Register::R10 => String::from("R10"),
            Register::DX => String::from("DX"),
            Register::R11 => String::from("R11"),
            Register::CX => String::from("CX"),
        }
    }

    fn pretty_printer_cc(cond_code: &CondCode) -> String {
        match cond_code {
            CondCode::E => String::from("E"),
            CondCode::NE => String::from("NE"),
            CondCode::G => String::from("G"),
            CondCode::GE => String::from("GE"),
            CondCode::L => String::from("L"),
            CondCode::LE => String::from("LE"),
        }
    }
}

fn tabs(indent_level: usize) -> String {
    "  ".repeat(indent_level)
}
