open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, compare, hash, to_string]

  val parser : t Angstrom.t
end

module Make (A : EXP_BY_SET_SPEC) = struct
  module Model_spec = Model_parsers.Exp_by_set.Make (A)
  module Formula_spec = Formula_parsers.Exp_by_set.Make (A)
  module Model = Model_parser.Make (Model_spec)
  module Formula = Formula_parser.Make (Formula_spec)

  let one_step_satisfaction ~model:_ ~box_or_diamond:_
      ~state:_ ~states:_ ~action:_ =
    (* Placeholder: will be implemented with actual game logic *)
    true

  let one_step_game ~model:_ ~state:_ ~modal_formula:_ :
      (Formula.t, State.t) Game.one_step_game =
    (* Placeholder: will be implemented with actual game logic *)
    { game = Hashtbl.Poly.create (); exit_nodes = [] }
end
