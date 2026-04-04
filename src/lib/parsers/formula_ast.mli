open! Core
open Naive_modcheck_coalg_common

(** AST for formulas - may not be in NNF *)
module type S = sig
  type 'a modality [@@deriving sexp]

  type t =
    | True
    | False
    | Ap of Ap.t
    | Not of t (* Not can negate any formula in AST *)
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Modal of t modality
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp]
end

type 'a relational_modality = Diamond of 'a | Box of 'a
[@@deriving sexp]

module Relational_ast :
  S with type 'a modality = 'a relational_modality

(** Probability threshold for probabilistic modal logic. For
    diamond <p/q>, the semantics is "probability > p/q". For
    box [p/q], the semantics is "probability <= p/q". *)

type 'a probabilistic_modality =
  | GE of Frac.t * 'a
  | LE of Frac.t * 'a
  | GT of Frac.t * 'a
  | LT of Frac.t * 'a
[@@deriving sexp]

module Probabilistic_ast :
  S with type 'a modality = 'a probabilistic_modality
