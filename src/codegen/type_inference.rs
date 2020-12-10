use std::collections::{HashMap, HashSet};

// Operation identifier, means that its name or a operator type.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum OperationId {
	Op_Add,
	Named(String),
}

// Operation, function or method that can be executed.
// Basically operations are functions.
pub struct Operation {
	id: OperationId,
	return_type: u64,
	parameter_type: Vec<u64>,
}

// Implementations of operations.
pub trait OperationImplementation {
	// TODO: Correct this function.
	fn emit_code(&self);
}

// Real, fully instantiated known types.
// NOTE: Types are fully instantiated, so they must provide implementations.
pub struct Type {
	name: String,
	operations: HashMap<OperationId, (Operation, Box<dyn OperationImplementation>)>,
}

// Abstract, (maybe) partially known types.
pub struct AbstractType {
	id: usize,
	operations: HashMap<OperationId, Operation>,
}

// impl AbstractType {
// 	pub fn make_superset_of(&mut self, type: &AbstractType) -> bool {

// 	}
// }

pub struct TypeGraph {
	types: Vec<Type>,
	abstract_types: Vec<AbstractType>,
	abstract_types_to_types: HashMap<usize, usize>,
	abstract_types_to_abstract_types: HashMap<usize, HashSet<usize>>,
}

// impl AbstractType {
// 	pub fn new(id: OperationId, return_type: usize, parameter_type: Vec<usize>) -> AbstractType {
// 		AbstractType {
// 			id,
// 			return_type,
// 			parameter_type,
// 		}
// 	}

// 	pub fn id(&self) -> &OperationId {
// 		&self.id
// 	}

// 	pub fn return_type(&self) -> usize {
// 		self.return_type
// 	}

// 	pub fn parameter_type(&self) -> &Vec<usize> {
// 		&self.parameter_type
// 	}
// }

// struct Ty {
// 	id: usize,
// 	operation_map: HashMap<OperationId, OperationType>,
// }

// impl Ty {
// 	pub fn new(id: usize) -> Ty {
// 		Ty {
// 			id,
// 			operation_map: HashMap::new(),
// 		}
// 	}

// 	pub fn id(&self) -> usize {
// 		self.id
// 	}

// 	pub fn operation_map(&self) -> &HashMap<OperationId, OperationType> {
// 		&self.operation_map
// 	}

// 	pub fn add_operation(
// 		&mut self,
// 		id: OperationId,
// 		return_type: usize,
// 		parameter_type: Vec<usize>,
// 	) {
// 		self.operation_map.insert(
// 			id.clone(),
// 			OperationType::new(id, return_type, parameter_type),
// 		);
// 	}

// 	pub fn add_operations_from(&mut self, from: &Ty) -> bool {}
// }

// impl TypeGraph {
// 	pub fn new() -> TypeGraph {
// 		TypeGraph {
// 			type_vec: Vec::new(),
// 			type_deps_map: HashMap::new(),
// 		}
// 	}

// 	pub fn ty(&self, id: usize) -> &Ty {
// 		&self.type_vec[id]
// 	}

// 	pub fn ty_mut(&mut self, id: usize) -> &mut Ty {
// 		&mut self.type_vec[id]
// 	}

// 	pub fn new_ty(&mut self) -> usize {
// 		let ty = Ty::new(self.type_vec.len());
// 		let ty_id = ty.id();
// 		self.type_vec.push(ty);
// 		self.type_deps_map.insert(ty_id, HashSet::new());

// 		ty_id
// 	}
// }
