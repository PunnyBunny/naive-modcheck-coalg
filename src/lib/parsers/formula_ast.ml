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

  val modal_map : (t -> t) -> t modality -> t modality
end

module Make (M : sig
  type 'a t [@@deriving sexp]

  val map : ('a -> 'a) -> 'a t -> 'a t
end) : S with type 'a modality = 'a M.t = struct
  type 'a modality = 'a M.t [@@deriving sexp]

  type t =
    | True
    | False
    | Ap of Ap.t
    | Not of t
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Modal of t modality
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp]

  let modal_map = M.map
end

type 'a relational_modality = Diamond of 'a | Box of 'a
[@@deriving sexp]

module Relational_ast = Make (struct
  type 'a t = 'a relational_modality [@@deriving sexp]

  let map f = function
    | Diamond x -> Diamond (f x)
    | Box x -> Box (f x)
end)

(** Probability threshold for probabilistic modal logic. For
    diamond <p/q>, the semantics is "probability > p/q". For
    box [p/q], the semantics is "probability <= p/q". *)

type 'a probabilistic_modality =
  | GE of Frac.t * 'a
  | LE of Frac.t * 'a
  | GT of Frac.t * 'a
  | LT of Frac.t * 'a
[@@deriving sexp]

module Probabilistic_ast = Make (struct
  type 'a t = 'a probabilistic_modality [@@deriving sexp]

  let map f = function
    | GE (p, x) -> GE (p, f x)
    | LE (p, x) -> LE (p, f x)
    | GT (p, x) -> GT (p, f x)
    | LT (p, x) -> LT (p, f x)
end)
