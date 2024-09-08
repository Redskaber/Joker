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
        struct Fn;
        impl Callable for Fn {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int - args)
            }
            fn arity(&self) -> usize {
                2
            }
        }
        Fn {}
    };
    let d = ||{
        struct Fn;
        impl Callable for Fn {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int + args)
            }
            fn arity(&self) -> usize {
                3
            }
        }
        Fn {}
    };
    println!("c: c.call(): {}, c.arity: {}", c().call(10, 100).unwrap(), c().arity());
    println!("d: d.call(): {}, d.arity: {}", d().call(20, 200).unwrap(), d().arity());

    struct LoxFn<F: Callable> {
        inner: F,
    }
    impl<F: Callable> LoxFn<F> {
        pub fn new(builder: impl Fn() -> LoxFn<F>) -> LoxFn<F> {
            builder()
        }
    }
    let aa = LoxFn::new(||{
        struct Fn;
        impl Callable for Fn {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int - args)
            }
            fn arity(&self) -> usize {
                2
            }
        }
        LoxFn::<Fn>{ inner: Fn{} }
    });

    let dd = LoxFn::new(||{
        struct Fn;
        impl Callable for Fn {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int * args)
            }
            fn arity(&self) -> usize {
                20
            }
        }
        LoxFn::<Fn>{ inner: Fn{} }
    });

    println!("aa: aa.call(): {}, aa.arity: {}", aa.inner.call(10, 100).unwrap(), aa.inner.arity());
    println!("dd: dd.call(): {}, dd.arity: {}", dd.inner.call(20, 200).unwrap(), dd.inner.arity());


    #[derive(Clone)]
    struct DFn {
        inner: Rc<dyn Callable>,
    }
    impl Debug for DFn {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "DFn")
        }
    }
    impl PartialEq for DFn {
        fn eq(&self, other: &Self) -> bool {
            Rc::ptr_eq(&self.inner, &other.inner)
        }
    }

    impl DFn {
        pub fn new(builder: impl Fn() -> Self) -> DFn {
            builder()
        }
    }
    impl Callable for DFn {
        fn call(&self, int: i32, args: i32) -> Result<i32, String> {
            self.inner.call(int, args)
        }
        fn arity(&self) -> usize {
            self.inner.arity()
        }
    }
    let df = ||{
        struct Fn;
        impl Callable for Fn {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int * args)
            }
            fn arity(&self) -> usize {
                10
            }
        }
        DFn{ inner: Rc::new(Fn{}) }
    };
    let ef = ||{
        struct Fn;
        impl Callable for Fn {
            fn call(&self, int: i32, args: i32) -> Result<i32, String> {
                Ok(int / args)
            }
            fn arity(&self) -> usize {
                20
            }
        }
        DFn{ inner: Rc::new(Fn{}) }
    };
    println!("df: df().call(10, 100).unwrap(): {}, df().arity(): {}", df().call(10, 100).unwrap(), df().arity());
    println!("ef: ef().call(20, 200).unwrap(): {}, ef().arity(): {}", ef().call(20, 200).unwrap(), ef().arity());
} 

pub fn ber() {
    let base: Vec<i32> = vec![1,3,5,7,9];
    // let mut diff: Vec<i32> = Vec::new();

    // calc previous and current diff
    
    // 1: xxxxx不推荐
    // for i in 1..base.len() {
    //     let current = base[i];
    //     let previous = base[i-1];
    //     diff.push(current-previous);
    // }

    //2: 可以
    // for [a, b] in base.array_windows::<2>().copied() {
    //     diff.push(a - b);
    // }

    // 3. 函数式 
    // let diff: Vec<i32> = base.array_windows::<2>() // nightly
    //     .copied()
    //     .map(|[a, b]| a-b)
    //     .collect();
    // println!("diff: {:?}",diff);
}
