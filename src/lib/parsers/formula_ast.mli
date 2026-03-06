open! Core
open Naive_modcheck_coalg_common

(** AST for formulas - may not be in NNF *)
module type S = sig
  type modality [@@deriving sexp]

  type t =
    | True
    | False
    | Ap of Ap.t
    | Not of t (* Not can negate any formula in AST *)
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Diamond of Action.t * modality * t
    | Box of Action.t * modality * t
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp]
end

module Relational_ast : S with type modality = unit
(** Relational logic AST: unit modality *)

(** Probability threshold for probabilistic modal logic. For diamond <p/q>, the
    semantics is "probability > p/q". For box [p/q], the semantics is
    "probability <= p/q". *)

module Probabilistic_ast : S with type modality = Frac.t
(** Probabilistic modal logic AST: fraction as modality *)
