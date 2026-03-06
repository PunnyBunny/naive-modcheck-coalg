open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics

type game_data = {
    num_nodes : int
  ; node_labels : string array
  ; node_owners : string array
        (** ["Eloise" | "Abelard"] per node *)
  ; node_priorities : int array
  ; node_successors : int list array
  ; winners : string array
        (** ["Eloise" | "Abelard"] per node *)
  ; strategy : int option array
        (** Strategy edge target, if any *)
  ; node_is_modal : bool array
  ; node_states : string list array
        (** State(s) per node: singleton for formula nodes,
            multiple for modal *)
  ; starting_node : int
  ; result : bool
}
(** Flat representation of a solved parity game for
    visualisation/export. *)

(** Interface for a concrete checker instance for a specific
    type of logic *)
module type S = sig
  module Logic : Logic.S

  val model_check :
       verbose:bool
    -> model:Logic.Model.t
    -> point:State.t
    -> formula:Logic.Formula.t
    -> bool

  val model_check_full :
       verbose:bool
    -> model:Logic.Model.t
    -> point:State.t
    -> formula:Logic.Formula.t
    -> game_data
end

module type Intf = sig
  module Make (L : Logic.S) : S with module Logic = L
end
