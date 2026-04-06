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
  'a Angstrom.t ->
  'b Angstrom.t ->
  ('a, 'b) Hashtbl.Poly.t Angstrom.t

(* ---- Generic model parser (functor) ---- *)

module Make (M : Model_intf.S) : sig
  val make_model_parser :
    transition:M.transition Angstrom.t -> string -> M.t
end
