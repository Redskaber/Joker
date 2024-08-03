mod build;

fn main() {
    if let Err(err) = build::generate_ast(&"src/".to_string()) {
        eprintln!("Err: {err}");
    }
}
