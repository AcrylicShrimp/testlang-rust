use std::vec::Vec;

#[derive(Debug, PartialEq)]
pub enum Type {
	EoF,
	Unknown,
	Colon,
	Semicolon,
	Id,
	Literal,
}

#[derive(Debug)]
pub struct Token {
	pub token_type: Type,
	pub token_content: String,
}

#[derive(Debug)]
pub struct Lexer {
	content: Vec<char>,
	index: usize,
	max_index: usize,
}

impl Lexer {
	pub fn new(content: String) -> Lexer {
		let char_vec: Vec<char> = content.chars().collect();
		let length = char_vec.len();

		Lexer {
			content: char_vec,
			index: 0,
			max_index: length,
		}
	}

	pub fn next(&mut self) -> Token {
		let mut content = self.next_blackspace().to_string();

		if content == "\0" {
			return Token {
				token_type: Type::EoF,
				token_content: "\0".to_string(),
			};
		}

		if content == ":" {
			return Token {
				token_type: Type::Colon,
				token_content: ":".to_string(),
			};
		}

		if content == ";" {
			return Token {
				token_type: Type::Semicolon,
				token_content: ";".to_string(),
			};
		}

		while !self.is_eof() && !self.content[self.index].is_whitespace() && !self.is_punctuation()
		{
			content.push(self.content[self.index]);
			self.index += 1;
		}

		let mut id = false;

		if content.starts_with("@") {
			id = true;
			content = content.chars().skip(1).collect();
		}

		if content.is_empty() {
			self.index += 1;
			return Token {
				token_type: Type::Unknown,
				token_content: self.content[self.index - 1].to_string(),
			};
		}

		Token {
			token_type: if id { Type::Id } else { Type::Literal },
			token_content: content,
		}
	}

	fn next_blackspace(&mut self) -> char {
		while !self.is_eof() && self.content[self.index].is_whitespace() {
			self.index += 1;
		}

		if self.is_eof() {
			return '\0';
		}

		self.index += 1;
		self.content[self.index - 1]
	}

	fn is_punctuation(&self) -> bool {
		match self.content[self.index] {
			':' | ';' | '@' => true,
			_ => false,
		}
	}

	fn is_eof(&self) -> bool {
		self.max_index <= self.index
	}
}
