open! Core
open Naive_modcheck_coalg_common

(** Common signature for all logics *)
module type S = sig
  type 'a modality [@@deriving sexp]
  type transition [@@deriving sexp]

  module Model :
    Model_intf.S with type transition = transition

  module Formula :
    Formula_intf.S with type 'a modality = 'a modality

  module Formula_ast :
    Naive_modcheck_coalg_parsers.Formula.Ast.S
      with type 'a modality = 'a modality

  val formula_of_ast : Formula_ast.t -> Formula.t
  (** Convert AST to formula (e.g., convert to NNF) *)

  type helper_functions = {
      theta : Var.t -> Formula.t option
          (** Returns the Mu/Nu subformula that contains a
              given variable *)
    ; alternation_depth : Var.t -> int option
          (** Returns ad(theta(X)) for a variable X *)
  }

  val one_step_satisfaction :
       model:Model.t
    -> box_or_diamond:[ `Box | `Diamond ]
    -> state:State.t
    -> states:State.t list
    -> action:Action.t
    -> bool
  (** Returns the solution to the one step satisfaction
      problem: whether `xi(s) \in [[♥]]U` (predicate
      lifting).

      - s is denoted by [state]
      - ♥ is denoted by [box_or_diamond] (TODO: and
        modality)
      - U is denoted by [states] *)

  val one_step_game :
       model:Model.t
    -> state:State.t
    -> modal_formula:Formula.t Formula.modality
    -> (Formula.t, State.t) Game.one_step_game

  val is_atom_in_state :
    model:Model.t -> state:State.t -> atom:Ap.t -> bool

  val get_states : model:Model.t -> State.t list
  val get_helper_functions : Formula.t -> helper_functions

  val parse_formula : string -> Formula_ast.t
  (** Parse a formula from a string *)

  val parse_model : string -> Model.t
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
module type LOGIC_SPECIFICATION = sig
  type 'a modality [@@deriving sexp]
  type transition [@@deriving sexp]

  module Model :
    Model_intf.S with type transition = transition

  module Formula :
    Formula_intf.S with type 'a modality = 'a modality

  module Formula_ast :
    Naive_modcheck_coalg_parsers.Formula.Ast.S
      with type 'a modality = 'a modality

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
    -> (Formula.t, State.t) Game.one_step_game

  val parse_formula : string -> Formula_ast.t
  val parse_model : string -> Model.t
end

module type Intf = sig
  module type S = S
  module type LOGIC_SPECIFICATION = LOGIC_SPECIFICATION

  module Make (Spec : LOGIC_SPECIFICATION) :
    S
      with type 'a modality = 'a Spec.modality
       and type transition = Spec.transition
       and module Model = Spec.Model
       and module Formula = Spec.Formula
       and module Formula_ast = Spec.Formula_ast
end
