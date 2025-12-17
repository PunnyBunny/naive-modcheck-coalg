open! Core
open Naive_modcheck_coalg_common
include Logic_intf

module Make (Spec : LOGIC_SPECIFICATION) : S = struct
  type model =
    ( State.t,
      Ap.t list * (Action.t, Spec.transition) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]

  type modality = Spec.modality [@@deriving sexp]

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
end
