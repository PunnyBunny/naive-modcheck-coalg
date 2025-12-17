open! Core
open Naive_modcheck_coalg_common

include Logic.Make (struct
  type transition = (State.t, int * int) Hashtbl.Poly.t [@@deriving sexp]

  type model =
    (State.t, Ap.t list * (Action.t, transition) Hashtbl.Poly.t) Hashtbl.Poly.t
  [@@deriving sexp]

  type comparison = Greater | Less | Equal [@@deriving sexp]
  type modality = comparison * (int * int) [@@deriving sexp]

  type formula =
    | True
    | False
    | Prop of Ap.t
    | Not of formula
    | And of formula * formula
    | Or of formula * formula
    | Diamond of Action.t * modality * formula
    | Box of Action.t * modality * formula
    | Mu of State.t * formula
    | Nu of State.t * formula
  [@@deriving sexp]
end)

type frac = int * int [@@deriving sexp]
