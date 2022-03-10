mod lang;
use lang::*;

fn main() -> std::io::Result<()> {
    let mut args = std::env::args();
    if args.len() == 1 {
        println!("Usage: parapet [FILE]");
        return Ok(());
    }
    args.next();

    let src = std::fs::read_to_string(args.next().unwrap()).expect("File not found.");

    let mut a = analyser::Analyser::analyse(&src);

    println!("{:#?}", a);

    Ok(())
}
