open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics

(** Interface for a concrete checker instance for a specific type of logic *)
module type S = sig
  module Logic : Logic.S

  val model_check :
    model:Logic.Model.t -> point:State.t -> formula:Logic.Formula.t -> bool
end

module type Intf = sig
  module Make (L : Logic.S) : S with module Logic = L
end
