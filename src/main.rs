use std::{
    env, fs,
    process::{Command, exit},
};
mod assembler;
mod code_emission;
mod lexer;
mod parser;
mod pretty_printer;
mod semantic_analysis;
mod tacky;

#[derive(Debug)]
enum ArgOption {
    None,
    Lex,
    Parser,
    Codegen,
    Source,
    Tacky,
    Validate,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("Not enough arguments passed in");
    }

    let print = args.len() >= 4 && args[3] == "--print";

    let (filename, option) = arg_parser(args, print);
    let c_file = format!("{}.c", filename);
    let preprocessed_file = format!("{}.i", filename);

    //preprocess
    Command::new("gcc")
        .args(["-E", "-P", &c_file, "-o", &preprocessed_file])
        .output()
        .unwrap();

    //compile
    let assembly_file = compile(&filename, option, print);
    fs::remove_file(preprocessed_file).unwrap();

    //linker
    if !assembly_file.is_empty() {
        if print {
            println!("Output file at {}", &filename);
        }
        Command::new("gcc")
            .args([assembly_file.clone(), "-o".to_string(), filename])
            .output()
            .unwrap();
        fs::remove_file(assembly_file).unwrap();
    }

    exit(0);
}

//input is of the form cargo run -- --option filename

fn arg_parser(args: Vec<String>, print: bool) -> (String, ArgOption) {
    let mut filename = args[1].split(".").next().unwrap().to_string();

    let option;
    if args.len() >= 3 {
        filename = args[2].split(".").next().unwrap().to_string();
        option = match args[1].as_str() {
            "--lex" => ArgOption::Lex,
            "--parse" => ArgOption::Parser,
            "--codegen" => ArgOption::Codegen,
            "-S" => ArgOption::Source,
            "--tacky" => ArgOption::Tacky,
            "--validate" => ArgOption::Validate,
            _ => {
                filename = args[1].split(".").next().unwrap().to_string();
                ArgOption::None
            }
        };
    } else {
        option = ArgOption::None;
    }
    if print {
        println!("Filename: {}.c", filename);

        println!("Option passed: {:?}", option);
    }

    (filename, option)
}

fn compile(filename: &str, option: ArgOption, print: bool) -> String {
    let filename = filename.to_string();
    match option {
        ArgOption::Lex => {
            let tokens = lexer::lexer(filename.clone());
            if print {
                println!("{:?}", tokens);
            }
            String::new()
        }
        ArgOption::Parser => {
            let tokens = lexer::lexer(filename.clone());
            let parsed_program = parser::parser(tokens);
            if print {
                println!(
                    "{}",
                    pretty_printer::parser::pretty_printer(&parsed_program, 0)
                );
            }
            String::new()
        }
        ArgOption::Validate => {
            let tokens = lexer::lexer(filename.clone());
            let mut parsed_program = parser::parser(tokens);
            semantic_analysis::semantic_analysis(&mut parsed_program);
            if print {
                println!(
                    "{}",
                    pretty_printer::parser::pretty_printer(&parsed_program, 0)
                );
            }
            String::new()
        }
        ArgOption::Tacky => {
            let tokens = lexer::lexer(filename.clone());
            let mut parsed_program = parser::parser(tokens);
            semantic_analysis::semantic_analysis(&mut parsed_program);
            let tacky_program = tacky::emit_tacky(parsed_program);
            if print {
                println!(
                    "{}",
                    pretty_printer::tacky::pretty_printer(&tacky_program, 0)
                );
            }
            String::new()
        }
        ArgOption::Codegen => {
            let tokens = lexer::lexer(filename.clone());
            let mut parsed_program = parser::parser(tokens);
            semantic_analysis::semantic_analysis(&mut parsed_program);
            let tacky_program = tacky::emit_tacky(parsed_program);
            let asm_program = assembler::assembler(tacky_program);
            if print {
                println!(
                    "{}",
                    pretty_printer::assembler::pretty_printer(&asm_program, 0)
                );
            }
            String::new()
        }
        ArgOption::Source => {
            let tokens = lexer::lexer(filename.clone());
            let mut parsed_program = parser::parser(tokens);
            semantic_analysis::semantic_analysis(&mut parsed_program);
            let tacky_program = tacky::emit_tacky(parsed_program);
            let asm_program = assembler::assembler(tacky_program);
            code_emission::code_emission(asm_program, filename);
            String::new()
        }
        ArgOption::None => {
            let tokens = lexer::lexer(filename.clone());
            let mut parsed_program = parser::parser(tokens);
            semantic_analysis::semantic_analysis(&mut parsed_program);
            let tacky_program = tacky::emit_tacky(parsed_program);
            let asm_program = assembler::assembler(tacky_program);
            code_emission::code_emission(asm_program, filename)
        }
    }
}
