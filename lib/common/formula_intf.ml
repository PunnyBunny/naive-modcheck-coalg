open! Core

(* TODO: NNF *)

(** Module type for formulas parameterized by modality *)
module type S = sig
  type modality [@@deriving sexp]

  type t =
    | True
    | False
    | Ap of Ap.t
    | Not of Ap.t
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Diamond of Action.t * modality * t
    | Box of Action.t * modality * t
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp]
end

(** Functor to create a formula module from a modality type *)
module Make (M : sig
  type t [@@deriving sexp]
end) : S with type modality = M.t = struct
  type modality = M.t [@@deriving sexp]

  type t =
    | True
    | False
    | Ap of Ap.t
    | Not of Ap.t
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Diamond of Action.t * modality * t
    | Box of Action.t * modality * t
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp]
end
(* TODO: AST != formula (NNF, other preprocessing) *)
