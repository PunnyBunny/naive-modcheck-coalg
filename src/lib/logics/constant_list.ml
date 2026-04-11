open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula
open Naive_modcheck_coalg_parsers_common.Lexer

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string, equal]

  val parser : t Angstrom.t
end

module Make (A : CONSTANT_SPEC) = struct
  module A_list = struct
    type t = A.t list [@@deriving sexp, equal]

    let to_string lst =
      let inner =
        lst
        |> List.map ~f:A.to_string
        |> String.concat ~sep:", "
      in
      Printf.sprintf "{%s}" inner

    let parser =
      let open Angstrom in
      kw "{" *> sep_by (kw ",") A.parser <* kw "}"
  end

  module Model_spec = Model_parsers.Constant.Make (A_list)
  module Formula_spec = Formula_parsers.Constant.Make (A)

  module One_step (M : sig
    type formula [@@deriving sexp]
    type state [@@deriving sexp]
  end) =
  struct
    type inner_node [@@deriving sexp]

    type one_step_node =
      | Start
      | Inner of inner_node
      | Exit of M.formula * M.state
    [@@deriving sexp]

    type one_step_game_t =
      ( one_step_node
      , Game.Player.t * Game.Priority.t * one_step_node list
      )
      Hashtbl.Poly.t
    [@@deriving sexp]

    let one_step_game
        ~transition:(lst : M.state Model_spec.t)
        ~(modal_formula : M.formula Formula_spec.t) :
        one_step_game_t =
      let game = Hashtbl.Poly.create () in
      let owner =
        match modal_formula with
        | Normal a when List.mem lst ~equal:A.equal a ->
            Game.Player.Abelard
        | Not a when not (List.mem lst ~equal:A.equal a) ->
            Game.Player.Abelard
        | _ -> Game.Player.Eloise
      in
      Hashtbl.set game ~key:Start ~data:(owner, 0, []);
      game
  end
end
