open! Core
open Naive_modcheck_coalg_common
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer
open Formula_parser
module P = Make (Formula_ast.Probabilistic_ast)

let modal formula =
  let frac open_ close_ mk =
    lift3
      (fun num denom subfmla ->
        mk (Frac.make num denom) subfmla)
      (kw open_ *> integer)
      (kw "/" *> integer <* kw close_)
      formula
  in
  frac "<" ">" (fun p f ->
      Formula_ast.Probabilistic_ast.Modal
        (Formula_ast.GT (p, f)))
  <|> frac "[" "]" (fun p f ->
      Formula_ast.Probabilistic_ast.Modal
        (Formula_ast.LE (p, f)))

let parse_probabilistic_formula =
  P.make_formula_parser ~modal
