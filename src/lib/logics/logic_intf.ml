open! Core
open Naive_modcheck_coalg_common

(** Common signature for all logics *)
module type S = sig
  type modality [@@deriving sexp]
  type transition [@@deriving sexp]
 
  module Model :
    Model_intf.S with type transition = transition

  module Formula :
    Formula_intf.S with type modality = modality

  module Formula_ast :
    Naive_modcheck_coalg_parsers.Formula.Ast.S
      with type modality = modality

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
      problem: whether `s \in [[♥]]U` (predicate lifting).

      - s is denoted by [state]
      - ♥ is denoted by [box_or_diamond] (TODO: and
        modality)
      - U is denoted by [states] *)

  val is_atom_in_state :
    model:Model.t -> state:State.t -> atom:Ap.t -> bool

  val get_states : model:Model.t -> State.t list
  val get_helper_functions : Formula.t -> helper_functions

  val parse_formula : string -> Formula_ast.t
  (** Parse a formula from a string *)

  val parse_model : string -> Model.t
  (** Parse a model from a string *)
end

module type LOGIC_SPECIFICATION = sig
  type modality [@@deriving sexp]
  type transition [@@deriving sexp]

  module Model :
    Model_intf.S with type transition = transition

  module Formula :
    Formula_intf.S with type modality = modality

  module Formula_ast :
    Naive_modcheck_coalg_parsers.Formula.Ast.S
      with type modality = modality

  val one_step_satisfaction :
       model:Model.t
    -> box_or_diamond:[ `Box | `Diamond ]
    -> state:State.t
    -> states:State.t list
    -> action:Action.t
    -> bool

  val parse_formula : string -> Formula_ast.t
  val parse_model : string -> Model.t
end

module type Intf = sig
  module type S = S
  module type LOGIC_SPECIFICATION = LOGIC_SPECIFICATION

  module Make (Spec : LOGIC_SPECIFICATION) :
    S
      with type modality = Spec.modality
       and type transition = Spec.transition
       and module Model = Spec.Model
       and module Formula = Spec.Formula
       and module Formula_ast = Spec.Formula_ast
end
