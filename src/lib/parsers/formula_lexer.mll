{
  exception Error of string
}

let al = ['a'-'z''A'-'Z']
let num = ['0'-'9']
let alnum = (al|num)+
let id = ['a'-'z''A'-'Z'](alnum | '_')*
let digit = ['0'-'9']+

rule lex = parse
	  | [' ' '\t']+ { lex lexbuf }
    | '\n' { Lexing.new_line lexbuf ; lex lexbuf }
    | "(*"_*"*)"  { lex lexbuf } (* eat comments *)
    | "tt" | "true" | "⊤" { Formula_parser.TRUE }
    | "ff" | "false" | "⊥" { Formula_parser.FALSE }
    | "/\\" | "&&" | "&" | "∧" { Formula_parser.AND }
    | "\\/" | "||" | "|" | "∨" { Formula_parser.OR }
    | "~" | "!" | "¬" { Formula_parser.NOT }
    | "mu" | "μ" { Formula_parser.MU }
    | "nu" | "ν" { Formula_parser.NU }
    | '<' { Formula_parser.LANGLE }
    | '>' { Formula_parser.RANGLE }
    | '[' { Formula_parser.LBRACK }
    | ']' { Formula_parser.RBRACK }

    | "/" { Formula_parser.SLASH }
    | digit as n { Formula_parser.INT (int_of_string n) }
    | "." { Formula_parser.DOT }
    | '(' { Formula_parser.LPAREN }
    | ')' { Formula_parser.RPAREN }
	  | eof { Formula_parser.EOF }
    | id as s { Formula_parser.IDENT s }
    | _ { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
