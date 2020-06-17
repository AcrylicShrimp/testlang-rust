extern crate strum;
extern crate strum_macros;

use strum_macros::Display;

use std::collections::HashMap;
use std::vec::Vec;

type TypeId = usize;

// Represents concrete actual types.
#[derive(Display, PartialEq, Clone)]
pub enum Type {
	Unknown,
	Void,
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
	Ty { id: usize },
	Abs { constraintVec: Vec<TypeConstraint> },
}

// Represents type constraints to describe abstract types.
#[derive(Display, PartialEq, Clone)]
pub enum TypeConstraint {
	PropGet {
		name: String,
		ty: Type,
	},
	PropSet {
		name: String,
		ty: Type,
	},
	Func {
		name: String,
		rty: Type,
		ptyVec: Vec<Type>,
	},
	Op, // TODO: Make a representation of operator overloadings.
}

// What is "Type"?
// Type is an abstract collection of property, methods, and operators.

pub struct ASTTypeEngine {
	type_vec: Vec<TypeId>,
	type_map: HashMap<TypeId, Type>,
	deps_map: HashMap<TypeId, Vec<TypeId>>,
}

impl ASTTypeEngine {
	pub fn new_type(&mut self) -> usize {
		let id = self.type_vec.len();

		self.type_vec.push(id);
		self.type_map.insert(id, Type::Unknown);

		id
	}

	pub fn add_constraint() {}

	fn update_type(&self) {}
}
