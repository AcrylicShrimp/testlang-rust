
// use some_module;
// use some_module as new_some_module;
// use some_module.some_function;
// use some_module.some_function as new_some_function;
// use some_module.{some_function, some_constant, some_constant_2};
// use some_module.{some_function as new_some_function, some_constant, some_constant_2};
// use some_module.{some_function as new_some_function, some_constant};

fn add(l, r) {
    ret (l + r);
}

fn do_something(sr, i) {
    ret sr as i32 + i;
}

let a;

if a + 2 == 10 {
	with b = a + 4 {
		let c = add(a, b) + add(a, add(a, b));
		c = do_something(c as str, c);
	}
}


// // This function and its value will be deduced in compile time.
// // This function is not a function template, hence the type placeholder T must be deduced into a single type.
// fn test(t) {
// 	ret t;
// }
//
// // This template function will instantiated in compile time.
// // The type placeholder T can be deduced into muliple types. The compiler will instantiate them.
// template test(t) {
// 	ret t;
// }
//
// // This public function can be seen by other module(file)s.
// pub fn test2() {
//
// }
//
// // This declares an external ffi function that can be called.
// extern test();
//
// // This declaration can be seen by other module(file)s.
// pub extern test();
//
// // This function will be marked to be exposed.
// pub extern test() {
//
// }
