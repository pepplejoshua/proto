use crate::{
    lexer::lexer::Lexer,
    source::{errors::CompileError, source::SourceFile},
};

pub fn compile_file(path: &str, run: bool) -> Result<(), CompileError> {
    // 1. Load source file
    let source = SourceFile::new(path.to_string())?;

    // 2. Create lexer
    let _lexer = Lexer::new(source);

    // 3. Create parser
    // let mut parser = Parser::new(lexer);

    // 4. Parse the file
    // let ast = parser.parse_file()?;

    // Report any lexer errors
    // if !parser.lex_errs.is_empty() {
    //     return Err(CompileError::LexicalErrors(parser.lex_errs));
    // }

    // Report any parser errors
    // if !parser.parse_errs.is_empty() {
    //     return Err(CompileError::ParsingErrors(parser.parse_errs));
    // }

    // 5. Type checking (will implement later)
    // let typed_ast = type_checker::check(ast)?;

    // 6. Code generation (will implement later)
    // let code = codegen::generate(typed_ast)?;

    // 7. If run=true, execute the code (will implement later)
    if run {
        //     execute::run(code)?;
    }

    Ok(())
}
