mod build;

pub fn tools_main() {
    let output_dir = "src".to_string();
    if let Err(err) = build::generate_ast(&output_dir) {
        eprintln!("err: {err}");
    }
}
