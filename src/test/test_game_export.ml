open! Core
open OUnit2
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics
open Naive_modcheck_coalg_checker
open Naive_modcheck_coalg_export

module Cases = struct
  type t = {
      logic : string
    ; model_str : string
    ; formula_str : string
    ; point : string
    ; game_data : Checker_intf.game_data
    ; direct_result : bool
  }

  let relational () =
    let model_str =
      {|[x: ({p1}, [a: {x, y}]), y: ({p2}, [a: {x, y}])]|}
    in
    let formula_str =
      "nu x2 . (mu x1 . (([@1]p1 & [@2][a]<> x1) | ([@1]p2 \
       & [@2][a][] x2)))"
    in
    let point = "x" in
    let model = Logics.Relational.parse_model model_str in
    let formula =
      Logics.Relational.parse_formula formula_str
    in
    let point_st = State.of_string point in
    {
      logic = "relational"
    ; model_str
    ; formula_str
    ; point
    ; game_data =
        Checkers.Relational.model_check_full ~verbose:false
          ~model ~point:point_st ~formula
    ; direct_result =
        Checkers.Relational.model_check ~verbose:false
          ~model ~point:point_st ~formula
    }

  let probabilistic () =
    let model_str =
      {|[x: ({p}, [a: [x: 2/3, y: 1/3]]), y: ({}, [a: [x: 1/2, y: 1/2]])]|}
    in
    let formula_str = "nu x . ([@1]p & [@2][a][>=1/2] x)" in
    let point = "x" in
    let model =
      Logics.Probabilistic.parse_model model_str
    in
    let formula =
      Logics.Probabilistic.parse_formula formula_str
    in
    let point_st = State.of_string point in
    {
      logic = "probabilistic"
    ; model_str
    ; formula_str
    ; point
    ; game_data =
        Checkers.Probabilistic.model_check_full
          ~verbose:false ~model ~point:point_st ~formula
    ; direct_result =
        Checkers.Probabilistic.model_check ~verbose:false
          ~model ~point:point_st ~formula
    }
end

module Assert = struct
  let equal_int = assert_equal ~printer:Int.to_string
  let equal_bool = assert_equal ~printer:Bool.to_string
  let equal_string = assert_equal ~printer:Fn.id
end

module Json = struct
  let string json field =
    Yojson.Safe.Util.(json |> member field |> to_string)

  let bool json field =
    Yojson.Safe.Util.(json |> member field |> to_bool)

  let int json field =
    Yojson.Safe.Util.(json |> member field |> to_int)

  let list json field =
    Yojson.Safe.Util.(json |> member field |> to_list)

  (** Every node JSON object must expose these fields with
      the given types — enforced by attempting the
      conversion. *)
  let node_fields =
    [
      ("id", `Int)
    ; ("formula", `String)
    ; ("owner", `String)
    ; ("priority", `Int)
    ; ("winner", `String)
    ; ("isStart", `Bool)
    ; ("isOneStep", `Bool)
    ; ("oneStepInfo", `String)
    ; ("stateOrTransition", `String)
    ]

  let assert_node_schema node =
    List.iter node_fields ~f:(fun (field, ty) ->
        match ty with
        | `Int -> ignore (int node field : int)
        | `Bool -> ignore (bool node field : bool)
        | `String -> ignore (string node field : string))
end

module Json_structure_test = struct
  open Cases
  open Assert

  let export_json case =
    Game_export.export_json ~game_data:case.game_data
      ~model_src:case.model_str
      ~formula_src:case.formula_str ~logic:case.logic
      ~point:case.point
    |> Yojson.Safe.pretty_to_string
    |> Yojson.Safe.from_string

  let run case _ctxt =
    let json = export_json case in
    let meta = Yojson.Safe.Util.member "metadata" json in
    equal_string case.logic (Json.string meta "logic");
    equal_string case.point (Json.string meta "point");
    equal_bool case.direct_result (Json.bool meta "result");
    equal_string case.formula_str
      (Json.string meta "formula");

    let nodes = Json.list json "nodes" in
    equal_int case.game_data.num_nodes (List.length nodes);
    assert_bool "nodes list non-empty"
      (not (List.is_empty nodes));
    List.iter nodes ~f:Json.assert_node_schema;

    let edges = Json.list json "edges" in
    assert_bool "edges list non-empty"
      (not (List.is_empty edges));
    assert_bool "at least one strategy edge"
      (List.exists edges ~f:(fun e ->
           Json.bool e "isStrategy"));

    let start_nodes =
      List.filter nodes ~f:(fun n -> Json.bool n "isStart")
    in
    equal_int 1 (List.length start_nodes)
end

module Game_data_consistency_test = struct
  open Cases
  open Assert

  let run case _ctxt =
    let gd = case.game_data in
    equal_bool case.direct_result gd.result;
    let per_node_arrays =
      [
        ("node_formulas", Array.length gd.node_formulas)
      ; ("node_owners", Array.length gd.node_owners)
      ; ("node_priorities", Array.length gd.node_priorities)
      ; ("node_successors", Array.length gd.node_successors)
      ; ("winners", Array.length gd.winners)
      ; ("strategy", Array.length gd.strategy)
      ; ( "node_one_step_info"
        , Array.length gd.node_one_step_info )
      ; ( "node_is_one_step"
        , Array.length gd.node_is_one_step )
      ; ( "node_states_or_transitions"
        , Array.length gd.node_states_or_transitions )
      ]
    in
    List.iter per_node_arrays ~f:(fun (name, len) ->
        assert_equal
          ~msg:(name ^ " length != num_nodes")
          ~printer:Int.to_string gd.num_nodes len);
    assert_bool "starting_node in range"
      (gd.starting_node >= 0
      && gd.starting_node < gd.num_nodes)
end

let tests_for ~name ~build =
  [
    ( name ^ ": JSON structure" >:: fun ctxt ->
      Json_structure_test.run (build ()) ctxt )
  ; ( name ^ ": game_data consistency" >:: fun ctxt ->
      Game_data_consistency_test.run (build ()) ctxt )
  ]

let suite =
  "Game_export"
  >::: List.concat
         [
           tests_for ~name:"Relational"
             ~build:Cases.relational
         ; tests_for ~name:"Probabilistic"
             ~build:Cases.probabilistic
         ]

let () = run_test_tt_main suite
