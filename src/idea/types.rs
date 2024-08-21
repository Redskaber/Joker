use obj_enum::{Class, ClassInstance};


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
        Fun(Function),
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

