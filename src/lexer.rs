extern crate strum;
extern crate strum_macros;

use std::vec::Vec;

use strum_macros::{Display, EnumString};

#[derive(PartialEq)]
enum AdvanceMode {
	Pre,
	Post,
	NoAdvance,
}

#[derive(PartialEq, Display, EnumString, Debug, Clone, Copy)]
pub enum TokenType {
	Unknown,
	Eof,
	Id,
	Dot,             // .
	Comma,           // ,
	Colon,           // :
	Semicolon,       // ;
	Question,        // ?
	Arrow,           // =>
	ParenL,          // (
	ParenR,          // )
	BraceL,          // {
	BraceR,          // }
	BracketL,        // [
	BracketR,        // ]
	LiteralBool,     // true false
	LiteralInteger,  // +1
	LiteralDecimal,  // 1.e-1
	LiteralString,   // '...'
	OpAdd,           // +
	OpSub,           // -
	OpMul,           // *
	OpDiv,           // /
	OpMod,           // %
	OpPow,           // **
	OpEq,            // ==
	OpNeq,           // !=
	OpLs,            // <
	OpLsEq,          // <=
	OpGt,            // >
	OpGtEq,          // >=
	OpOr,            // or
	OpAnd,           // and
	OpNot,           // not
	OpBitOr,         // |
	OpBitAnd,        // &
	OpBitXor,        // ^
	OpBitNot,        // ~
	OpShiftL,        // <<
	OpShiftR,        // >>
	OpAssign,        // =
	OpAssignAdd,     // +=
	OpAssignSub,     // -=
	OpAssignMul,     // *=
	OpAssignDiv,     // /=
	OpAssignMod,     // %=
	OpAssignPow,     // **=
	OpAssignBitOr,   // |=
	OpAssignBitAnd,  // &=
	OpAssignBitXor,  // ^=
	OpAssignBitNot,  // ~=
	OpAssignShiftL,  // <<=
	OpAssignShiftR,  // >>=
	KeywordAs,       // as
	KeywordLet,      // let
	KeywordRet,      // ret
	KeywordBreak,    // break
	KeywordContinue, // continue
	KeywordIf,       // if
	KeywordElse,     // else
	KeywordFor,      // for
	KeywordIn,       // in
	KeywordFrom,     // from
	KeywordWith,     // with
	KeywordVoid,     // void
	KeywordBool,     // bool
	KeywordI8,       // i8
	KeywordI16,      // i16
	KeywordI32,      // i32
	KeywordI64,      // i64
	KeywordI128,     // i128
	KeywordU8,       // u8
	KeywordU16,      // u16
	KeywordU32,      // u32
	KeywordU64,      // u64
	KeywordU128,     // u128
	KeywordF16,      // f16
	KeywordF32,      // f32
	KeywordF64,      // f64
	KeywordStr,      // str
}

#[derive(Debug)]
pub struct Token {
	pub token_type: TokenType,
	pub token_content: String,
	pub line_number: usize,
	pub line_offset: usize,
}

pub struct Lexer {
	content: Vec<char>,
	index: usize,
	max_index: usize,
	line_offset: usize,
	line_number: usize,
}

impl Lexer {
	pub fn new(content: String) -> Lexer {
		let char_vec: Vec<char> = content.chars().collect();
		let length = char_vec.len();

		Lexer {
			content: char_vec,
			index: 0,
			max_index: length,
			line_offset: 1,
			line_number: 1,
		}
	}

	fn is_eof(&self) -> bool {
		return self.max_index <= self.index;
	}

	fn ch(&self) -> char {
		return self.content[self.index];
	}

	fn is_whitespace(&self) -> bool {
		return self.ch().is_whitespace();
	}

	fn is_punctuation(&self) -> bool {
		return self.ch() != '_' && self.ch().is_ascii_punctuation();
	}

	fn is_newline(&self) -> bool {
		return self.ch() == '\n';
	}

	fn pick_blackspace(&mut self) -> char {
		loop {
			if self.is_eof() {
				return '\0';
			}

			if !self.is_whitespace() {
				break;
			}

			self.line_offset += 1;

			if self.is_newline() {
				self.line_offset = 0;
				self.line_number += 1;
			}

			self.index += 1;
		}

		self.ch()
	}

	fn next_character(&mut self, advance_mode: AdvanceMode) -> char {
		if self.is_eof() {
			return '\0';
		}

		if advance_mode == AdvanceMode::Pre {
			if !self.is_eof() {
				self.line_offset += 1;

				if self.is_newline() {
					self.line_offset = 0;
					self.line_number += 1;
				}

				self.index += 1;
			}

			return if self.is_eof() { '\0' } else { self.ch() };
		}

		let character = self.ch();

		if advance_mode == AdvanceMode::Post {
			if !self.is_eof() {
				self.line_offset += 1;

				if self.is_newline() {
					self.line_offset = 0;
					self.line_number += 1;
				}

				self.index += 1;
			}
		}

		character
	}

	fn parse_number(&mut self) -> Option<Token> {
		let index = self.index;
		let line_offset = self.line_offset;

		fn read_integer(this: &mut Lexer) -> String {
			let mut integer = "".to_string();

			while this.next_character(AdvanceMode::NoAdvance).is_digit(10) {
				integer.push(this.next_character(AdvanceMode::Post));
			}

			integer
		};
		fn read_exp(this: &mut Lexer) -> String {
			match this.next_character(AdvanceMode::NoAdvance) {
				'e' | 'E' => (),
				_ => {
					return "".to_string();
				}
			}

			let index = this.index;
			let line_offset = this.line_offset;

			let mut exp = this.next_character(AdvanceMode::Post).to_string();

			match this.next_character(AdvanceMode::NoAdvance) {
				'+' | '-' => {
					exp.push(this.next_character(AdvanceMode::Post));
				}
				_ => (),
			}

			if !this.next_character(AdvanceMode::NoAdvance).is_digit(10) {
				this.index = index;
				this.line_offset = line_offset;

				return "".to_string();
			}

			return exp + &read_integer(this);
		};
		fn return_none(this: &mut Lexer, index: usize, line_offset: usize) -> Option<Token> {
			this.index = index;
			this.line_offset = line_offset;

			return None;
		};
		fn return_integer(this: &Lexer, integer: String, line_offset: usize) -> Option<Token> {
			Some(Token {
				token_type: TokenType::LiteralInteger,
				token_content: integer,
				line_offset: line_offset,
				line_number: this.line_number,
			})
		};
		fn return_decimal(this: &Lexer, decimal: String, line_offset: usize) -> Option<Token> {
			Some(Token {
				token_type: TokenType::LiteralDecimal,
				token_content: decimal,
				line_offset: line_offset,
				line_number: this.line_number,
			})
		};

		let mut sign = "".to_string();

		match self.next_character(AdvanceMode::NoAdvance) {
			'+' | '-' => {
				sign.push(self.next_character(AdvanceMode::Post));
			}
			_ => (),
		}

		match self.next_character(AdvanceMode::NoAdvance) {
			'.' => {
				self.next_character(AdvanceMode::Pre);

				let integer = read_integer(self);

				if integer.is_empty() {
					return_none(self, index, line_offset)
				} else {
					let exp = read_exp(self);

					return_decimal(self, sign + "." + &integer + &exp, line_offset)
				}
			}
			'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
				let integer = read_integer(self);
				let mut decimal = "".to_string();

				if self.next_character(AdvanceMode::NoAdvance) == '.' {
					decimal =
						self.next_character(AdvanceMode::Post).to_string() + &read_integer(self);
				}

				decimal += &read_exp(self);

				if decimal.is_empty() {
					return_integer(self, sign + &integer, line_offset)
				} else {
					return_decimal(self, sign + &integer + &decimal, line_offset)
				}
			}
			_ => return_none(self, index, line_offset),
		}
	}

	pub fn next(&mut self) -> Token {
		let mut token = Token {
			token_type: TokenType::Unknown,
			token_content: "".to_string(),
			line_number: self.line_number,
			line_offset: self.line_offset,
		};

		let return_token = |token_type: TokenType, token_content: String| -> Token {
			token.token_type = token_type;
			token.token_content = token_content;

			token
		};

		let blackspace = self.pick_blackspace();

		if blackspace == '\0' {
			return return_token(TokenType::Eof, "".to_string());
		}

		match blackspace {
			'.' => match self.parse_number() {
				Some(token) => {
					return token;
				}
				None => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::Dot, ".".to_string());
				}
			},
			',' => {
				self.next_character(AdvanceMode::Pre);
				return return_token(TokenType::Comma, blackspace.to_string());
			}
			':' => {
				self.next_character(AdvanceMode::Pre);
				return return_token(TokenType::Colon, blackspace.to_string());
			}
			';' => {
				self.next_character(AdvanceMode::Pre);
				return return_token(TokenType::Semicolon, blackspace.to_string());
			}
			'?' => {
				self.next_character(AdvanceMode::Pre);
				return return_token(TokenType::Question, blackspace.to_string());
			}
			'(' => {
				self.next_character(AdvanceMode::Pre);
				return return_token(TokenType::ParenL, blackspace.to_string());
			}
			')' => {
				self.next_character(AdvanceMode::Pre);
				return return_token(TokenType::ParenR, blackspace.to_string());
			}
			'{' => {
				self.next_character(AdvanceMode::Pre);
				return return_token(TokenType::BraceL, blackspace.to_string());
			}
			'}' => {
				self.next_character(AdvanceMode::Pre);
				return return_token(TokenType::BraceR, blackspace.to_string());
			}
			'[' => {
				self.next_character(AdvanceMode::Pre);
				return return_token(TokenType::BracketL, blackspace.to_string());
			}
			']' => {
				self.next_character(AdvanceMode::Pre);
				return return_token(TokenType::BracketR, blackspace.to_string());
			}
			'+' => match self.next_character(AdvanceMode::Pre) {
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpAssignAdd, "+=".to_string());
				}
				_ => {
					self.index -= 1;
					self.line_offset -= 1;

					return match self.parse_number() {
						Some(token) => {
							return token;
						}
						None => {
							self.next_character(AdvanceMode::Pre);
							return_token(TokenType::OpAdd, "+".to_string())
						}
					};
				}
			},
			'-' => match self.next_character(AdvanceMode::Pre) {
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpAssignSub, "-=".to_string());
				}
				_ => {
					self.index -= 1;
					self.line_offset -= 1;

					return match self.parse_number() {
						Some(token) => {
							return token;
						}
						None => {
							self.next_character(AdvanceMode::Pre);
							return_token(TokenType::OpSub, "-".to_string())
						}
					};
				}
			},
			'*' => match self.next_character(AdvanceMode::Pre) {
				'*' => match self.next_character(AdvanceMode::Pre) {
					'=' => {
						self.next_character(AdvanceMode::Pre);
						return return_token(TokenType::OpAssignPow, "**=".to_string());
					}
					_ => {
						return return_token(TokenType::OpPow, "**".to_string());
					}
				},
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpAssignMul, "*=".to_string());
				}
				_ => {
					return return_token(TokenType::OpMul, "*".to_string());
				}
			},
			'/' => match self.next_character(AdvanceMode::Pre) {
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpAssignDiv, "/=".to_string());
				}
				_ => {
					return return_token(TokenType::OpDiv, "/".to_string());
				}
			},
			'%' => match self.next_character(AdvanceMode::Pre) {
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpAssignMod, "%=".to_string());
				}
				_ => {
					return return_token(TokenType::OpMod, "%".to_string());
				}
			},
			'=' => match self.next_character(AdvanceMode::Pre) {
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpEq, "==".to_string());
				}
				'>' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::Arrow, "=>".to_string());
				}
				_ => {
					return return_token(TokenType::OpAssign, "=".to_string());
				}
			},
			'!' => {
				if self.next_character(AdvanceMode::Pre) == '=' {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpNeq, "!=".to_string());
				} else {
					self.index -= 1;
					self.line_offset -= 1;
				}
			}
			'<' => match self.next_character(AdvanceMode::Pre) {
				'<' => match self.next_character(AdvanceMode::Pre) {
					'=' => {
						self.next_character(AdvanceMode::Pre);
						return return_token(TokenType::OpAssignShiftL, "<<=".to_string());
					}
					_ => {
						return return_token(TokenType::OpShiftL, "<<".to_string());
					}
				},
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpLsEq, "<=".to_string());
				}
				_ => {
					return return_token(TokenType::OpLs, "<".to_string());
				}
			},
			'>' => match self.next_character(AdvanceMode::Pre) {
				'>' => match self.next_character(AdvanceMode::Pre) {
					'=' => {
						self.next_character(AdvanceMode::Pre);
						return return_token(TokenType::OpAssignShiftR, ">>=".to_string());
					}
					_ => {
						return return_token(TokenType::OpShiftR, ">>".to_string());
					}
				},
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpGtEq, ">=".to_string());
				}
				_ => {
					return return_token(TokenType::OpGt, ">".to_string());
				}
			},
			'|' => match self.next_character(AdvanceMode::Pre) {
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpAssignBitOr, "|=".to_string());
				}
				_ => {
					return return_token(TokenType::OpBitOr, "|".to_string());
				}
			},
			'&' => match self.next_character(AdvanceMode::Pre) {
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpAssignBitAnd, "&=".to_string());
				}
				_ => {
					return return_token(TokenType::OpBitAnd, "&".to_string());
				}
			},
			'^' => match self.next_character(AdvanceMode::Pre) {
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpAssignBitXor, "^=".to_string());
				}
				_ => {
					return return_token(TokenType::OpBitXor, "^".to_string());
				}
			},
			'~' => match self.next_character(AdvanceMode::Pre) {
				'=' => {
					self.next_character(AdvanceMode::Pre);
					return return_token(TokenType::OpAssignBitNot, "~=".to_string());
				}
				_ => {
					return return_token(TokenType::OpBitNot, "~".to_string());
				}
			},
			'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
				match self.parse_number() {
					Some(token) => {
						return token;
					}
					None => (),
				}
			}
			'\'' => {
				self.next_character(AdvanceMode::Pre);

				let mut string = String::new();

				while !self.is_eof() {
					match self.ch() {
						'\\' => {
							self.next_character(AdvanceMode::Pre);

							if self.is_eof() {
								panic!("unexpected eof reached.");
							}

							match self.ch() {
								'n' => string.push('\n'),
								'r' => string.push('\r'),
								't' => string.push('\t'),
								'\\' => string.push('\\'),
								'0' => string.push('\0'),
								'\'' => string.push('\''),
								'"' => string.push('"'),
								'`' => string.push('`'),
								_ => string.push(self.ch()),
							}

							self.next_character(AdvanceMode::Pre);
						}
						'\'' => {
							break;
						}
						_ => string.push(self.next_character(AdvanceMode::Post)),
					}
				}

				if self.ch() != '\'' {
					panic!("expected ' not detected");
				}

				self.next_character(AdvanceMode::Pre);

				return return_token(TokenType::LiteralString, string);
			}
			_ => (),
		}

		if self.is_punctuation() {
			return return_token(
				TokenType::Unknown,
				self.next_character(AdvanceMode::Post).to_string(),
			);
		}

		let mut content = String::new();

		while !self.is_eof() && !self.is_whitespace() && !self.is_punctuation() {
			content.push(self.next_character(AdvanceMode::Post));
		}

		match content.as_ref() {
			"true" => return_token(TokenType::LiteralBool, content),
			"false" => return_token(TokenType::LiteralBool, content),
			"or" => return_token(TokenType::OpOr, content),
			"and" => return_token(TokenType::OpAnd, content),
			"not" => return_token(TokenType::OpNot, content),
			"as" => return_token(TokenType::KeywordAs, content),
			"let" => return_token(TokenType::KeywordLet, content),
			"ret" => return_token(TokenType::KeywordRet, content),
			"break" => return_token(TokenType::KeywordBreak, content),
			"continue" => return_token(TokenType::KeywordContinue, content),
			"if" => return_token(TokenType::KeywordIf, content),
			"else" => return_token(TokenType::KeywordElse, content),
			"for" => return_token(TokenType::KeywordFor, content),
			"in" => return_token(TokenType::KeywordIn, content),
			"from" => return_token(TokenType::KeywordFrom, content),
			"with" => return_token(TokenType::KeywordWith, content),
			"void" => return_token(TokenType::KeywordVoid, content),
			"bool" => return_token(TokenType::KeywordBool, content),
			"i8" => return_token(TokenType::KeywordI8, content),
			"i16" => return_token(TokenType::KeywordI16, content),
			"i32" => return_token(TokenType::KeywordI32, content),
			"i64" => return_token(TokenType::KeywordI64, content),
			"i128" => return_token(TokenType::KeywordI128, content),
			"u8" => return_token(TokenType::KeywordU8, content),
			"u16" => return_token(TokenType::KeywordU16, content),
			"u32" => return_token(TokenType::KeywordU32, content),
			"u64" => return_token(TokenType::KeywordU64, content),
			"u128" => return_token(TokenType::KeywordU128, content),
			"f16" => return_token(TokenType::KeywordF16, content),
			"f32" => return_token(TokenType::KeywordF32, content),
			"f64" => return_token(TokenType::KeywordF64, content),
			"str" => return_token(TokenType::KeywordStr, content),
			_ => return_token(TokenType::Id, content),
		}
	}
}
