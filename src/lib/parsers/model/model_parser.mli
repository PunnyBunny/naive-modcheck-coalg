open! Core
open Naive_modcheck_coalg_common

(* ---- Model-specific tokens ---- *)

val state : State.t Angstrom.t
val ap : Ap.t Angstrom.t
val action : Action.t Angstrom.t
val frac : Frac.t Angstrom.t

(* ---- Grammar combinators ---- *)

val list_of : 'a Angstrom.t -> 'a list Angstrom.t

val func_of :
     'a Angstrom.t
  -> 'b Angstrom.t
  -> ('a, 'b) Hashtbl.Poly.t Angstrom.t

(* ---- Generic model parser (functor) ---- *)

module type SPEC = sig
  type 'a t [@@deriving sexp] (* The functor *)

  val to_string :
    'a t -> to_string_inner:('a -> string) -> string

  val parser : model_inner:'a Angstrom.t -> 'a t Angstrom.t
end

module Make (M : SPEC) :
  Model.S with type 'a transition = 'a M.t
