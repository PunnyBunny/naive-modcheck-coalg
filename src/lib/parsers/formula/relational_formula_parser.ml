open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer
open Formula_parser
module P = Make (Formula_ast.Relational_ast)

let box x =
  Formula_ast.Relational_ast.Modal (Formula_ast.Box x)

let diamond x =
  Formula_ast.Relational_ast.Modal (Formula_ast.Diamond x)

let modal formula =
  kw "[]" *> formula
  >>| box
  <|> (kw "<>" *> formula >>| diamond)

let parse_relational_formula = P.make_formula_parser ~modal
