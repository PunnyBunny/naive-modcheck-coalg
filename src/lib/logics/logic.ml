open! Core
open Naive_modcheck_coalg_common
include Logic_intf

module Make (Spec : LOGIC_SPECIFICATION) :
  S
    with module Model_spec = Spec.Model_spec
     and module Formula_spec = Spec.Formula_spec = struct
  module Model_spec = Spec.Model_spec
  module Formula_spec = Spec.Formula_spec
  module Model = Spec.Model
  module Formula = Spec.Formula

  let parse_model = Model.parse_model
  let parse_formula = Formula.parse_formula

  type helper_functions = {
      theta : Var.t -> Formula.t option
    ; alternation_depth : Var.t -> int option
  }

  let one_step_satisfaction = Spec.one_step_satisfaction
  let one_step_game = Spec.one_step_game
  let is_atom_in_state = Spec.is_atom_in_state

  let get_states ~(model : Model.t) = Model.states model

  let get_theta formula =
    let theta_table = Hashtbl.Poly.create () in
    let rec build : Formula.t -> unit = function
      | True | False | Var _ -> ()
      | And (f1, f2) | Or (f1, f2) -> build f1; build f2
      | Modal m ->
          ignore (Formula_spec.map m ~f:(fun sub -> build sub; sub))
      | (Mu (v, f) | Nu (v, f)) as self ->
          Hashtbl.set theta_table ~key:v ~data:self;
          build f
    in
    build formula;
    fun v -> Hashtbl.find theta_table v

  let get_alternation_depth formula =
    let ad_table = Hashtbl.Poly.create () in
    let rec build : Formula.t -> int = function
      | True | False | Var _ -> 0
      | And (f1, f2) | Or (f1, f2) -> Int.max (build f1) (build f2)
      | Modal m ->
          let max_depth = ref 0 in
          ignore
            (Formula_spec.map m ~f:(fun sub ->
                 max_depth := Int.max !max_depth (build sub);
                 sub));
          !max_depth
      | Mu (v, f) ->
          let d = build f in
          let d' = if d mod 2 = 0 then d + 1 else d in
          Hashtbl.set ad_table ~key:v ~data:d';
          d'
      | Nu (v, f) ->
          let d = build f in
          let d' = if d mod 2 = 1 then d + 1 else d in
          Hashtbl.set ad_table ~key:v ~data:d';
          d'
    in
    ignore (build formula);
    fun v -> Hashtbl.find ad_table v

  let get_helper_functions formula =
    { theta = get_theta formula
    ; alternation_depth = get_alternation_depth formula
    }
end
