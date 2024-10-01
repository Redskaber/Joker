mod build;

fn main() {
    if let Err(err) = build::generate_ast(&"tools/src/".to_string()) {
        eprintln!("Err: {err}");
    }
}
