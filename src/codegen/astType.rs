extern crate strum;
extern crate strum_macros;

use strum_macros::Display;

type TypeId = usize;

// Represents types.
#[derive(Display, PartialEq, Clone)]
pub enum Type {
	Bool,
	I8,
	I16,
	I32,
	I64,
	I128,
	U8,
	U16,
	U32,
	U64,
	U128,
	F16,
	F32,
	F64,
	Str,
	// Tuple(Vec<Type>),
	// NamedTuple(Vec<(String, Type)>),
	// Func(Vec<Type>, Option<Box<Type>>),
	// Superpositioned(Vec<Type>),
}
