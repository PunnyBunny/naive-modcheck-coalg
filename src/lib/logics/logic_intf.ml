open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

(** Common signature for all logics *)
module type S = sig
  module Model_spec : Model_parser.SPEC
  module Formula_spec : Formula_parser.SPEC

  (* module Model :
    Model.S with type 'a transition = 'a Model_spec.t

  module Formula :
    Formula.S with type 'a modality = 'a Formula_spec.t *)

  (* type helper_functions = {
      theta : Var.t -> Formula.t option
          (** Returns the Mu/Nu subformula that contains a
              given variable *)
    ; alternation_depth : Var.t -> int option
          (** Returns ad(theta(X)) for a variable X *)
  }

  
  type inner_node

  type one_step_node =
    | Start
    | Inner of inner_node
    | Exit of M.formula * M.state

  type one_step_game_t =
    ( one_step_node
    , Game.Player.t * Game.Priority.t * one_step_node list
    )
    Hashtbl.Poly.t

  val one_step_satisfaction :
       model:Model.t
    -> box_or_diamond:[ `Box | `Diamond ]
    -> state:State.t
    -> states:State.t list
    -> action:Action.t
    -> bool

  val one_step_game :
       model:Model.t
    -> state:State.t
    -> modal_formula:Formula.t Formula.modality
    -> ( 'formula
       , 'state
       , ('formula, 'state) inner_node )
       one_step_game_t

  val get_states : model:Model.t -> State.t list
  val get_helper_functions : Formula.t -> helper_functions

  val parse_formula : string -> Formula.t
  (** Parse a formula from a string *)

  val parse_model : string -> Model.t *)
  (** Parse a model from a string *)
end

(*
TODO:
  * modality no longer is just box/diamond
  * one step satisfaction change from solving
      xi(c) \in [[♥]]U, i.e. whether c |= ♥ U under (C, xi : C -> T C, c),
    to giving (c, phi) as starting node, omit U,
    construct the one-step game/arena (by providing a builder function),
    e.g. T = Pow: (c, box phi) --A-> (d, phi) for all d in xi(c)
    e.g. T = Dist:
    - (c, L_p phi) --E-> (D, phi), xi(c)(D) >= p
    - (D, phi) --A-> (d, phi), d in D
  * recover one step satisfaction by
    - (c, ♥ phi) --E-> (D, phi), xi(c) \in [[♥]]D
    - (D, phi) --A-> (d, phi), d in D

Plan:
  * migrate to
    - one_step_game : model -> state -> formula -> builder -> unit
    - outermost operator of formula is a modal operator
  * change the checkers to use the one step game instead of one step satisfaction
  * regress
  * think about the syntax enabled by this, e.g. remove atoms and encode with (\otimes At)
*)
module type LOGIC_SPECIFICATION = functor
  (M : sig
     type formula
     type state (* TODO: abstract this module argument *)
   end)
  -> sig
  module Model_spec : Model_parser.SPEC
  module Formula_spec : Formula_parser.SPEC

  type inner_node

  type one_step_node =
    | Start
    | Inner of inner_node
    | Exit of M.formula * M.state

  type one_step_game_t =
    ( one_step_node
    , Game.Player.t * Game.Priority.t * one_step_node list
    )
    Hashtbl.Poly.t

  (* 'state = X, 'formula = L *)
  val one_step_game :
       transition:M.state Model_spec.t
    -> modal_formula:M.formula Formula_spec.t
    -> one_step_game_t
end

module type Intf = sig
  module type S = S
  module type LOGIC_SPECIFICATION = LOGIC_SPECIFICATION

  (* module Make (Spec : LOGIC_SPECIFICATION) :
    S
      with module Model_spec = Spec.Model_spec
       and module Formula_spec = Spec.Formula_spec *)
end
