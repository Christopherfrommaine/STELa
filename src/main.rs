mod parser;
mod interpreter;

const PRELUDE: &str = include_str!("prelude.st");

fn main() {
    let path = std::env::args().into_iter().skip(1).next().unwrap_or("examples/test1.st".to_string());
    let file_contents = std::fs::read_to_string(path);

    match file_contents {
        Ok(s) => {
            let l = parser::Lexer::new(&(s));
            let mut p = parser::Parser::new(l);
            let res = p.parse();
            println!("{:?}", res)
        },
        Err(e) => {eprintln!("No file found:\n{:?}", e);},
    }
}
