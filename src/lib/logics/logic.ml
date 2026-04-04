open! Core
open Naive_modcheck_coalg_common
include Logic_intf

module Make (Spec : LOGIC_SPECIFICATION) :
  S
    with type 'a modality = 'a Spec.modality
     and type transition = Spec.transition
     and module Model = Spec.Model
     and module Formula = Spec.Formula
     and module Formula_ast = Spec.Formula_ast = struct
  type 'a modality = 'a Spec.modality [@@deriving sexp]
  type transition = Spec.transition [@@deriving sexp]

  module Model = Spec.Model
  module Formula = Spec.Formula
  module Formula_ast = Spec.Formula_ast

  (** Convert AST to NNF formula by pushing negations inward
  *)
  let rec formula_of_ast (ast : Formula_ast.t) : Formula.t =
    match ast with
    | True -> True
    | False -> False
    | Ap p -> Ap p
    | Var x -> Var x
    | And (f1, f2) ->
        And (formula_of_ast f1, formula_of_ast f2)
    | Or (f1, f2) ->
        Or (formula_of_ast f1, formula_of_ast f2)
    | Modal m ->
        Modal (Formula.map_modality formula_of_ast m)
    | Mu (x, f) -> Mu (x, formula_of_ast f)
    | Nu (x, f) -> Nu (x, formula_of_ast f)
    | Not f -> negate (formula_of_ast f)

  (** Negate a formula, pushing the negation inward to
      maintain NNF *)
  and negate (f : Formula.t) : Formula.t =
    match f with
    | True -> False
    | False -> True
    | Ap p -> Not p
    | Not p -> Ap p
    | Var _ -> failwith "Cannot negate a fixpoint variable"
    | And (f1, f2) -> Or (negate f1, negate f2)
    | Or (f1, f2) -> And (negate f1, negate f2)
    | Modal m -> Modal (Formula.negate_modality negate m)
    | Mu (x, f) -> Nu (x, negate f)
    | Nu (x, f) -> Mu (x, negate f)

  type helper_functions = {
      theta : Var.t -> Formula.t option
    ; alternation_depth : Var.t -> int option
  }

  let one_step_satisfaction = Spec.one_step_satisfaction
  let one_step_game = Spec.one_step_game

  let is_atom_in_state ~(model : Model.t) ~(state : State.t)
      ~(atom : Ap.t) : bool =
    match Hashtbl.find model state with
    | None -> false
    | Some (atoms, _) -> List.mem atoms atom ~equal:Ap.equal

  let get_states ~(model : Model.t) : State.t list =
    Hashtbl.to_alist model |> List.map ~f:fst

  let get_theta formula =
    let theta_table = Hashtbl.Poly.create () in
    let rec build_theta_table : Formula.t -> unit = function
      | True
      | False
      | Ap _
      | Not _ ->
          ()
      | Var _ -> ()
      | And (f1, f2)
      | Or (f1, f2) ->
          build_theta_table f1;
          build_theta_table f2
      | Modal m -> Formula.iter_modality build_theta_table m
      | (Mu (v, f') | Nu (v, f')) as f ->
          Hashtbl.set theta_table ~key:v ~data:f;
          build_theta_table f'
    in
    build_theta_table formula;
    fun v -> Hashtbl.find theta_table v

  let get_alternation_depth formula =
    let alternation_depth_table = Hashtbl.Poly.create () in
    let rec build_table : Formula.t -> int = function
      | True
      | False
      | Ap _
      | Not _
      | Var _ ->
          0
      | And (f1, f2)
      | Or (f1, f2) ->
          Int.max (build_table f1) (build_table f2)
      | Modal m ->
          let max_depth = ref 0 in
          Formula.iter_modality
            (fun f ->
              max_depth :=
                Int.max !max_depth (build_table f))
            m;
          !max_depth
      | Mu (v, f) ->
          let depth = build_table f in
          let new_depth =
            if depth mod 2 = 0 then depth + 1 else depth
          in
          Hashtbl.set alternation_depth_table ~key:v
            ~data:new_depth;
          new_depth
      | Nu (v, f) ->
          let depth = build_table f in
          let new_depth =
            if depth mod 2 = 1 then depth + 1 else depth
          in
          Hashtbl.set alternation_depth_table ~key:v
            ~data:new_depth;
          new_depth
    in
    ignore (build_table formula);
    fun f -> Hashtbl.find alternation_depth_table f

  let get_helper_functions formula =
    {
      theta = get_theta formula
    ; alternation_depth = get_alternation_depth formula
    }

  let parse_formula = Spec.parse_formula
  let parse_model = Spec.parse_model
end
