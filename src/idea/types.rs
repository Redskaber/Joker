use std::{cell::Ref, collections::HashMap};

use obj_enum::{Class, ClassInstance};
use type_scope::{Array, Set, Str, I32};


mod obj_enum {
    use std::collections::HashMap;

    #[derive(Debug)]
    pub(super)enum Object {
        Literal(Literal),
        Caller(Caller),
        Instance(Instance),
    }

    #[derive(Debug)]
    pub(super) enum Literal {
        I32(i32),
        F64(f64),
        Str(String),
        Bool(bool),
        Null,
    }

    #[derive(Debug)]
    pub(super) enum Caller {
        Fn(Function),
        Lambda(Lambda),
        Class(Class),
    }

    #[derive(Debug)]
    pub(super) enum Function {
        Native(NativeFunction),
        User(UserFunction),
    }

    #[derive(Debug)]
    pub(super) struct NativeFunction;
    #[derive(Debug)]
    pub(super) struct UserFunction;
    #[derive(Debug)]
    pub(super) struct Lambda;
    #[derive(Debug)]
    pub(super) struct Class {
        pub(super) name: String,
        pub(super) methods: Option<HashMap<String, UserFunction>>,
    }

    #[derive(Debug)]
    pub(super) enum Instance {
        Class(ClassInstance),
    }

    #[derive(Debug)]
    pub(super) struct  ClassInstance {
        pub(super) class: Class,
        pub(super) fields: Option<HashMap<String, Object>>
    }
}

mod obj_type {
    use std::{cell::{Ref, RefCell, RefMut}, rc::Rc};

    use super::obj_enum::Object as OEnum;

    #[derive(Debug)]
    pub(super) struct Object {
        inner: Rc<RefCell<OEnum>>
    }

    impl Object {
        pub fn new(inner: OEnum) -> Self {
            Object { inner: Rc::new(RefCell::new(inner)) }
        }
        pub fn set(&self, inner: OEnum) {
            *self.inner.borrow_mut() = inner;
        }
        pub fn get(&self) -> Ref<OEnum> {
            self.inner.borrow()
        }
        pub fn get_mut(&self) ->RefMut<OEnum> {
            self.inner.borrow_mut()
        }
    }

}

pub fn types_main() {
    use obj_enum::Object as OEnum;
    use obj_type::Object;


    let obj = Object::new(OEnum::Literal(obj_enum::Literal::I32(100)));
    println!("obj: {:?}", obj);
    println!("obj get: {:?}", obj.get());
    println!("obj get_mut: {:?}", obj.get_mut());
    
    *obj.get_mut() =OEnum::Caller(obj_enum::Caller::Class(Class { name: String::from("Demo"), methods: None }));
    println!("obj: {:?}", obj);
    println!("obj get: {:?}", obj.get());
    println!("obj get_mut: {:?}", obj.get_mut());

    obj.set(OEnum::Instance(obj_enum::Instance::Class(ClassInstance { 
        class: Class { name: String::from("Demo"), methods: None }, 
        fields: None,
    })));
    println!("obj: {:?}", obj);
    println!("obj get: {:?}", obj.get());
    println!("obj get_mut: {:?}", obj.get_mut());
    
}



    mod type_scope {
        use std::marker::PhantomData;
    
        #[derive(Debug)]
        pub enum Type<'a> {
            I32(I32),
            Str(Str<'a>),
            Set(Set<'a, Type<'a>>),
            Array(Array<'a, Type<'a>>),
        }
    
        #[derive(Debug)]
        pub struct I32(pub i32);
    
        #[derive(Debug)]
        pub struct Str<'a>(pub &'a str);
    
        #[derive(Debug)]
        pub struct Set<'a, T: Sized + 'a>(pub &'a [T]);
    
        #[derive(Debug)]
        pub struct Array<'a, T: Sized + 'a>(pub &'a [T], PhantomData<&'a T>);
    
        impl<'a, T: Sized + 'a> Array<'a, T> {
            pub fn new(data: &'a [T]) -> Self {
                Array(data, PhantomData)
            }
        }
    }
    

pub fn type_manager() {
    use type_scope::Type;
    // let set = type_scope::Type::Set(Box::new([
    //     type_scope::Type::I32,
    //     type_scope::Type::Str,
    //     type_scope::Type::Set(Box::new([
    //         type_scope::Type::I32,
    //     ])),
    // ]));

    let set_i32: Set<'_, I32> = Set::<I32>(&[I32(100), I32(200), I32(300)]);
    let set_str: Set<'_, Str> = Set::<Str>(&[Str("100"), Str("200"), Str("300")]);
    let set_type: Set<'_, Type> = Set::<Type>(&[
        Type::I32(I32(100)),
        Type::Str(Str("100")),
        Type::Set(Set(&[
            Type::I32(I32(200)),
            Type::Str(Str("200")),
            Type::Set(Set(&[
                Type::I32(I32(300)),
                Type::Str(Str("300")),
            ]))
        ])),
    ]);
    
    let d = Set::<Type>(&[
        Type::Array(Array::new(&[
            Type::I32(I32(1000)),
            Type::Str(Str("300")),  // this need error, don't same type
        ]))
    ]);


    println!("set: {set_i32:#?}");
    println!("set: {set_str:#?}");
    println!("set: {set_type:#?}");
    // println!("array: {array:#?}");
}




