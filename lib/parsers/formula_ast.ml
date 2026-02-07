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

module Make (M : sig
  type t [@@deriving sexp]
end) : S with type modality = M.t = struct
  type modality = M.t [@@deriving sexp]

  type t =
    | True
    | False
    | Ap of Ap.t
    | Not of t
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Diamond of Action.t * modality * t
    | Box of Action.t * modality * t
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp]
end

(** Relational logic AST: unit modality *)
module Relational_ast = Make (struct
  type t = unit [@@deriving sexp]
end)

type frac = Frac.t [@@deriving sexp]
(** Probability threshold for probabilistic modal logic. For diamond <p/q>, the
    semantics is "probability > p/q". For box [p/q], the semantics is
    "probability <= p/q". *)

(** Probabilistic modal logic AST: fraction as modality *)
module Probabilistic_ast = Make (struct
  type t = frac [@@deriving sexp]
end)
