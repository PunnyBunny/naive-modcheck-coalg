open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers

(** Relational modal logic *)

module Relational_formula : Formula_intf.S with type modality = unit
module Relational_model : Model_intf.S with type transition = State.t list

include
  Logic.S
    with type modality = unit
     and type transition = State.t list
     and module Formula = Relational_formula
     and module Model = Relational_model
     and module Formula_ast = Formula.Ast.Relational_ast
