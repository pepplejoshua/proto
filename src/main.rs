use std::env;

use compiler::CompileContext;

mod compiler;
mod lexer;
mod parser;
mod pastel;
mod source;

const USAGE: &str = "
Usage: proto [command | filename.pr]

commands:
    - r | run path
    - b | build path
    - h | help: show this help message.
";

fn show_help() {
    println!("{}", USAGE);
}

fn main() {
    let args = env::args().collect::<Vec<String>>();

    if args.len() < 2 {
        show_help();
        return;
    }

    // get the command / filename, which is the first argument
    // after program name
    match args[1].as_str() {
        "h" | "help" => {
            show_help();
        }
        "r" | "run" => {
            // run project mode: proto run PROJECT_PATH
            let project_path = if let Some(project_path) = args.get(2) {
                if project_path == "." {
                    std::env::current_dir().unwrap()
                } else {
                    project_path.into()
                }
            } else {
                std::env::current_dir().unwrap()
            };

            match CompileContext::new_project(project_path, true) {
                Ok(mut ctx) => {
                    if let Err(e) = ctx.compile() {
                        eprintln!("Compilation error: {:?}", e);
                    }
                }
                Err(e) => {
                    eprintln!("Failed to create project context: {:?}", e);
                }
            }
        }
        "b" | "build" => {
            // build project mode: proto build PROJECT_PATH
            let project_path = if let Some(project_path) = args.get(2) {
                if project_path == "." {
                    std::env::current_dir().unwrap()
                } else {
                    project_path.into()
                }
            } else {
                std::env::current_dir().unwrap()
            };

            match CompileContext::new_project(project_path, false) {
                Ok(mut ctx) => {
                    if let Err(e) = ctx.compile() {
                        eprintln!("Compilation error: {:?}", e);
                    }
                }
                Err(e) => {
                    eprintln!("Failed to create project context: {:?}", e);
                }
            }
        }
        filename if filename.ends_with(".pr") => {
            // script mode: proto script.pr
            match CompileContext::new_script(filename.into()) {
                Ok(mut ctx) => {
                    if let Err(e) = ctx.compile() {
                        eprintln!("Compilation error: {:?}", e);
                    }
                }
                Err(e) => {
                    eprintln!("Failed to create project context: {:?}", e);
                }
            }
        }
        _ => {
            eprintln!("Error: Unknown command or invalid file name");
            show_help();
        }
    }
}
