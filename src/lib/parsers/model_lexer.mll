{
  exception Error of string
}

let al = ['a'-'z''A'-'Z']
let num = ['0'-'9']
let alnum = (al|num)+
let int = '-'?num+
let id = ['a'-'z'](alnum | '_')* (* to support models from mcrl2 idetifieers need to contain spechial characters *)

rule lex = parse
	  | [' ' '\t']+ { lex lexbuf }
    | '\n' { Lexing.new_line lexbuf ; lex lexbuf }
    | "(*"_*"*)"  { lex lexbuf } (* eat comments *)
	  | ':' { Model_parser.COLON }
	  | ',' { Model_parser.COMMA }
	  | '[' { Model_parser.LBRACK }
	  | ']' { Model_parser.RBRACK }
	  | '(' { Model_parser.LNBRACE }
	  | ')' { Model_parser.RNBRACE }
	  | '{' { Model_parser.LCBRACE }
	  | '}' { Model_parser.RCBRACE }
	  | '/' { Model_parser.SLASH }
    | id as s { Model_parser.IDENT s }
    | '"'   { read_string (Buffer.create 17) lexbuf }
	  | int as n { Model_parser.INT(int_of_string n)}
	  | eof { Model_parser.EOF }
and read_string buf =
  parse
  | '"'       { Model_parser.IDENT ("\"" ^ (Buffer.contents buf) ^ "\"") }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Error ("String is not terminated")) }
