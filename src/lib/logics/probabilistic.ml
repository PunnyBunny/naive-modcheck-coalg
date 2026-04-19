open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_common.Lexer
open Angstrom

module Actions = struct
  include Specs.Actions

  let parser =
    kw "{" *> kw "}" *> return (Action.of_string "")
    <|> Specs.Actions.parser
end

include
  Logic.Make
    (Product.Make
       (Constant_list.Make
          (Specs.Ap))
          (Exp_by_set.Make (Actions) (Distribution)))
