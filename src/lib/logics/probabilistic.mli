open! Core
open Naive_modcheck_coalg_common
module Parsers = Naive_modcheck_coalg_parsers

(** Probabilistic modal logic with probability thresholds. For diamond <p/q>,
    semantics is "probability > p/q". For box [p/q], semantics is "probability
    <= p/q". *)

module Probabilistic_formula :
  Formula_intf.S
    with type 'a modality = 'a Parsers.Formula.Ast.probabilistic_modality

module Probabilistic_model :
  Model_intf.S with type transition = (State.t, Frac.t) Hashtbl.Poly.t

include
  Logic.S
    with type 'a modality = 'a Parsers.Formula.Ast.probabilistic_modality
     and type transition = (State.t, Frac.t) Hashtbl.Poly.t
     and module Formula = Probabilistic_formula
     and module Model = Probabilistic_model
     and module Formula_ast = Parsers.Formula.Ast.Probabilistic_ast
