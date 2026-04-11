open! Core

module type S = sig
  type 'a modality

  type t =
    | True
    | False
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Modal of t modality
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp, to_string]

  val parse_formula : string -> t
end
