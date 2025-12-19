open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics

module type S = sig
  module Logic : Logic.S

  val model_check :
    model:Logic.model -> point:State.t -> formula:Logic.formula -> bool
end

module type Intf = sig
  module Make (L : Logic.S) : S with module Logic = L
end
