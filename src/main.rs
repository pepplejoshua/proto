use std::env;

mod compiler;
mod lexer;
mod parser;
mod pastel;
mod source;

const USAGE: &str = "
proto v0.1
usage: proto <command> [arguments]

Commands:
    compile <file.pr>     Compile a single file
    run <file.pr>         Compile and run a file
    help                  Show this help message
";

#[derive(Debug)]
enum Command {
    Compile(String), // path to file
    Run(String),     // path to file
    Help,
}

fn parse_args() -> Result<Command, String> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return Ok(Command::Help);
    }

    match args[1].as_str() {
        "help" | "--help" | "-h" => Ok(Command::Help),

        "compile" | "c" => {
            if args.len() < 3 {
                return Err("Missing file argument for compile command".into());
            }
            let path = &args[2];
            if !path.ends_with(".pr") {
                return Err("Source file must have .pr extension".into());
            }
            Ok(Command::Compile(path.clone()))
        }

        "run" | "r" => {
            if args.len() < 3 {
                return Err("Missing file argument for run command".into());
            }
            let path = &args[2];
            if !path.ends_with(".pr") {
                return Err("Source file must have .pr extension".into());
            }
            Ok(Command::Run(path.clone()))
        }

        _ => Err("Unknown command. Use 'proto help' for usage information.".into()),
    }
}

fn main() {
    match parse_args() {
        Ok(command) => match command {
            Command::Help => {
                println!("{}", USAGE);
            }

            Command::Compile(file) => {
                println!("Compiling {}", file);
                match compiler::compile_file(&file, false) {
                    Ok(_) => println!("Compilation successful"),
                    Err(e) => eprintln!("Compilation failed: {}", e),
                }
            }

            Command::Run(file) => {
                println!("Running {}", file);
                match compiler::compile_file(&file, true) {
                    Ok(_) => (), // program has run
                    Err(e) => eprintln!("Execution failed: {}", e),
                }
            }
        },

        Err(error) => {
            eprintln!("Error: {}", error);
            println!("\n{}", USAGE);
        }
    }
}
