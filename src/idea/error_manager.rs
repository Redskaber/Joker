//! This file if error manager idea rs
//! 
//! 
//! 


mod error {
    pub(crate) enum Error {
        Scanner(scanner::Error),
        Parser(parser::Error),
        Resolver(resolver::Error),
        Env(env::Error),
    }
    pub mod scanner {
        pub (crate) enum Error {
            InitError,
            LineError,
            Sub(sub_scanner::Error),
        }
        pub mod sub_scanner {
            pub (crate) enum Error {
                SubError,
                EndError,
            }   
        }
    }
    mod parser {
        pub(crate) enum Error {
            BaseError,
            EndError,
        }
    }
    mod resolver {
        pub(crate) enum Error {
            EnvError,
            StructError,
        }
    }
    mod env {
        pub(crate) enum Error {
            EnvError,
            FindError,
        }
    }
}



fn used() {
    use error::Error;
    use error::scanner::Error::InitError;
    use error::scanner::Error::Sub;
    use error::scanner::sub_scanner::Error::EndError;
    let scanner_init_error = Error::Scanner(InitError);
    let sub_scanner_end_err = Error::Scanner(Sub(EndError));
}


pub fn error_manager() {
    let scanner_init_error = error::Error::Scanner(
        error::scanner::Error::InitError
    );
}