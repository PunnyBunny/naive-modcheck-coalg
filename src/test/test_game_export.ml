open! Core
open OUnit2
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics
open Naive_modcheck_coalg_checker
open Naive_modcheck_coalg_export

let rel_model_str =
  {|[x: ({p1}, [{}: {x, y}]), y: ({p2}, [{}: {x, y}])]|}

let rel_formula_str =
  "nu x2 . (mu x1 . ((p1 & <> x1) | (p2 & [] x2)))"

type test_setup = {
    model : Logics.Relational.Model.t
  ; formula : Logics.Relational.Formula.t
  ; game_data : Checker_intf.game_data
}

let setup _ctxt =
  let model = Logics.Relational.parse_model rel_model_str in
  let formula =
    Logics.Relational.parse_formula rel_formula_str
  in
  let game_data =
    Checkers.Relational.model_check_full ~verbose:false
      ~model
      ~point:(State.of_string "x")
      ~formula
  in
  { model; formula; game_data }

let teardown _setup _ctxt = ()

(* TODO: make the test more readable, e.g. abstract away assert equals *)
let test_json_structure ctxt =
  let { game_data; _ } = bracket setup teardown ctxt in
  let json =
    Game_export.export_json ~game_data
      ~model_src:rel_model_str ~formula_src:rel_formula_str
      ~logic:"relational" ~point:"x"
  in
  let json_str = Yojson.Safe.pretty_to_string json in
  let parsed = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  (* Metadata *)
  let meta = parsed |> member "metadata" in
  assert_equal ~printer:Fn.id "relational"
    (meta |> member "logic" |> to_string);
  assert_equal ~printer:Fn.id "x"
    (meta |> member "point" |> to_string);
  assert_equal ~printer:Bool.to_string true
    (meta |> member "result" |> to_bool);
  assert_equal ~printer:Fn.id rel_formula_str
    (meta |> member "formula" |> to_string);
  (* Nodes *)
  let nodes = parsed |> member "nodes" |> to_list in
  assert_equal ~printer:Int.to_string game_data.num_nodes
    (List.length nodes);
  assert_bool "should have at least one node"
    (List.length nodes > 0);
  (* All nodes have required fields *)
  List.iter nodes ~f:(fun n ->
      ignore (n |> member "id" |> to_int : int);
      ignore (n |> member "formula" |> to_string : string);
      ignore (n |> member "owner" |> to_string : string);
      ignore (n |> member "priority" |> to_int : int);
      ignore (n |> member "winner" |> to_string : string);
      ignore (n |> member "isStart" |> to_bool : bool);
      ignore (n |> member "isOneStep" |> to_bool : bool);
      ignore
        (n |> member "oneStepInfo" |> to_string : string);
      ignore
        (n |> member "stateOrTransition" |> to_string
          : string));
  (* Edges *)
  let edges = parsed |> member "edges" |> to_list in
  assert_bool "should have edges" (List.length edges > 0);
  (* At least one strategy edge *)
  let has_strategy =
    List.exists edges ~f:(fun e ->
        e |> member "isStrategy" |> to_bool)
  in
  assert_bool "should have at least one strategy edge"
    has_strategy;
  (* Exactly one starting node *)
  let start_nodes =
    List.filter nodes ~f:(fun n ->
        n |> member "isStart" |> to_bool)
  in
  assert_equal ~printer:Int.to_string 1
    (List.length start_nodes)

(* TODO: fix the test *)
let test_game_data_result ctxt =
  let { model; formula; game_data } =
    bracket setup teardown ctxt
  in
  (* model_check_full result should match model_check *)
  let result =
    Checkers.Relational.model_check ~verbose:false ~model
      ~point:(State.of_string "x")
      ~formula
  in
  assert_equal ~printer:Bool.to_string result
    game_data.result;
  (* num_nodes matches array lengths *)
  assert_equal ~printer:Int.to_string game_data.num_nodes
    (Array.length game_data.node_formulas);
  assert_equal ~printer:Int.to_string game_data.num_nodes
    (Array.length game_data.node_owners);
  assert_equal ~printer:Int.to_string game_data.num_nodes
    (Array.length game_data.node_priorities);
  assert_equal ~printer:Int.to_string game_data.num_nodes
    (Array.length game_data.node_successors);
  assert_equal ~printer:Int.to_string game_data.num_nodes
    (Array.length game_data.winners);
  assert_equal ~printer:Int.to_string game_data.num_nodes
    (Array.length game_data.strategy);
  assert_equal ~printer:Int.to_string game_data.num_nodes
    (Array.length game_data.node_states_or_transitions);
  (* starting_node is in range *)
  assert_bool "starting_node in range"
    (game_data.starting_node >= 0
    && game_data.starting_node < game_data.num_nodes)

let suite =
  "Game_export"
  >::: [
         "JSON structure" >:: test_json_structure
       ; "game_data consistency" >:: test_game_data_result
       ]

let () = run_test_tt_main suite
