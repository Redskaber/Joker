#[derive(Debug)]
pub struct JokerError {
    line: usize,
    msg: String,
}

impl JokerError {
    pub fn error(line: usize, msg: String) -> JokerError {
        JokerError { line, msg }
    }

    pub fn report(&self, loc: String) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, loc, self.msg
        );
    }
}
