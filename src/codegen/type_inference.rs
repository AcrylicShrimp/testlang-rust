use std::collections::{HashMap, HashSet};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum FunctionId {
	Named(String),
}

pub struct Function {
	pub id: FunctionId,
	pub return_type: usize,
	pub parameter_type: Vec<usize>,
}

pub struct Trait {
	pub index: usize,
	pub name: String,
	pub functions: HashMap<FunctionId, Function>,
}

pub struct Struct {
	pub index: usize,
	pub name: String,
	pub fields: HashMap<String, usize>,
	pub functions: HashMap<FunctionId, Function>,
	pub implementations: Vec<Trait>,
}

pub enum Type {
	Trait(Trait),
	Struct(Struct),
}

pub enum AbstractType {
	Trait(Vec<usize>),
	Struct(usize),
}

pub struct TypeGraph {
	traits: Vec<Trait>,
	structs: Vec<Struct>,
	trait_names: HashMap<String, usize>,
	struct_names: HashMap<String, usize>,
	next_abstract_type: usize,
	abstract_types_to_types: Vec<Option<AbstractType>>,
	abstract_types_to_abstract_types: Vec<HashSet<usize>>,
}

impl TypeGraph {
	pub fn new(mut traits: Vec<Trait>, mut structs: Vec<Struct>) -> TypeGraph {
		for index in 0..traits.len() {
			traits[index].index = index;
		}

		for index in 0..structs.len() {
			structs[index].index = index;
		}

		let mut trait_names = HashMap::new();
		let mut struct_names = HashMap::new();

		for r#trait in &traits {
			trait_names.insert(r#trait.name.clone(), r#trait.index);
		}

		for r#struct in &structs {
			struct_names.insert(r#struct.name.clone(), r#struct.index);
		}

		TypeGraph {
			traits,
			structs,
			trait_names,
			struct_names,
			next_abstract_type: 0,
			abstract_types_to_types: Vec::new(),
			abstract_types_to_abstract_types: Vec::new(),
		}
	}

	pub fn trait_by_name(&self, name: &str) -> Option<&Trait> {
		self.trait_names.get(name).map(|&index| &self.traits[index])
	}
	pub fn struct_by_name(&self, name: &str) -> Option<&Struct> {
		self.struct_names
			.get(name)
			.map(|&index| &self.structs[index])
	}

	pub fn new_abstract_type(&mut self) -> usize {
		let abstract_type = self.next_abstract_type;
		self.next_abstract_type += 1;
		self.abstract_types_to_types.push(None);
		self.abstract_types_to_abstract_types.push(HashSet::new());
		abstract_type
	}

	pub fn specify_abstract_type_as_trait(
		&mut self,
		abstract_type: usize,
		r#trait: usize,
	) -> Result<(), ()> {
		match &mut self.abstract_types_to_types[abstract_type] {
			Some(r#type) => match r#type {
				AbstractType::Trait(abstract_type_traits) => {
					for &trait_index in abstract_type_traits.iter() {
						if trait_index == r#trait {
							return Ok(());
						}
					}

					abstract_type_traits.push(r#trait);
					Ok(())
				}
				&mut AbstractType::Struct(abstract_type_struct) => {
					for implemented_trait in &self.structs[abstract_type_struct].implementations {
						if implemented_trait.index == r#trait {
							return Ok(());
						}
					}

					Err(())
				}
			},
			None => {
				self.abstract_types_to_types[abstract_type] =
					Some(AbstractType::Trait(vec![r#trait]));
				Ok(())
			}
		}
	}

	pub fn specify_abstract_type_as_struct(
		&mut self,
		abstract_type: usize,
		r#struct: usize,
	) -> Result<(), ()> {
		match &mut self.abstract_types_to_types[abstract_type] {
			Some(r#type) => match r#type {
				AbstractType::Trait(abstract_type_traits) => {
					'trait_loop: for &r#trait in abstract_type_traits.iter() {
						for implemented_trait in &self.structs[r#struct].implementations {
							if implemented_trait.index == r#trait {
								continue 'trait_loop;
							}
						}

						return Err(());
					}

					*r#type = AbstractType::Struct(r#struct);
					Ok(())
				}
				&mut AbstractType::Struct(abstract_type_struct) => {
					if abstract_type_struct == r#struct {
						Ok(())
					} else {
						Err(())
					}
				}
			},
			None => {
				self.abstract_types_to_types[abstract_type] = Some(AbstractType::Struct(r#struct));
				Ok(())
			}
		}
	}

	pub fn specify_sub_type(&mut self, super_type: usize, sub_type: usize) {
		self.abstract_types_to_abstract_types[super_type].insert(sub_type);
	}

	pub fn resolve_types(mut self) -> Result<(), ()> {
		Ok(())
	}
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
