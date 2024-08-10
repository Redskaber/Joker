//! This file is try rust alias 
//! 
//! 

use std::{fmt::Debug, rc::Rc};
// idea?

pub fn d() {
    trait Callable {
        fn call(&self, int: i32, args: i32) -> Result<i32, String>;
        fn arity(&self) -> usize;
    }

    let c = ||{
        struct Fun;
        impl Callable for Fun {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int - args)
            }
            fn arity(&self) -> usize {
                2
            }
        }
        Fun {}
    };
    let d = ||{
        struct Fun;
        impl Callable for Fun {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int + args)
            }
            fn arity(&self) -> usize {
                3
            }
        }
        Fun {}
    };
    println!("c: c.call(): {}, c.arity: {}", c().call(10, 100).unwrap(), c().arity());
    println!("d: d.call(): {}, d.arity: {}", d().call(20, 200).unwrap(), d().arity());

    struct LoxFun<F: Callable> {
        fun: F,
    }
    impl<F: Callable> LoxFun<F> {
        pub fn new(fun: impl Fn() -> LoxFun<F>) -> LoxFun<F> {
            fun()
        }
    }
    let aa = LoxFun::new(||{
        struct Fun;
        impl Callable for Fun {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int - args)
            }
            fn arity(&self) -> usize {
                2
            }
        }
        LoxFun::<Fun>{fun: Fun{}}
    });

    let dd = LoxFun::new(||{
        struct Fun;
        impl Callable for Fun {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int * args)
            }
            fn arity(&self) -> usize {
                20
            }
        }
        LoxFun::<Fun>{fun: Fun{}}
    });

    println!("aa: aa.call(): {}, aa.arity: {}", aa.fun.call(10, 100).unwrap(), aa.fun.arity());
    println!("dd: dd.call(): {}, dd.arity: {}", dd.fun.call(20, 200).unwrap(), dd.fun.arity());


    #[derive(Clone)]
    struct DFun {
        fun: Rc<dyn Callable>,
    }
    impl Debug for DFun {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "DFun")
        }
    }
    impl PartialEq for DFun {
        fn eq(&self, other: &Self) -> bool {
            Rc::ptr_eq(&self.fun, &other.fun)
        }
    }

    impl DFun {
        pub fn new(builder: impl Fn() -> Self) -> DFun {
            builder()
        }
    }
    impl Callable for DFun {
        fn call(&self, int: i32, args: i32) -> Result<i32, String> {
            self.fun.call(int, args)
        }
        fn arity(&self) -> usize {
            self.fun.arity()
        }
    }
    let df = ||{
        struct Fun;
        impl Callable for Fun {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int * args)
            }
            fn arity(&self) -> usize {
                10
            }
        }
        DFun{fun: Rc::new(Fun{})}
    };
    let ef = ||{
        struct Fun;
        impl Callable for Fun {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int / args)
            }
            fn arity(&self) -> usize {
                20
            }
        }
        DFun{fun: Rc::new(Fun{})}
    };
    println!("df: df().call(10, 100).unwrap(): {}, df().arity(): {}", df().call(10, 100).unwrap(), df().arity());
    println!("ef: ef().call(20, 200).unwrap(): {}, ef().arity(): {}", ef().call(20, 200).unwrap(), ef().arity());
} 
