open! Core
open OUnit2
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics
open Naive_modcheck_coalg_checker

(* Parse a .test file into a list of (logic, formula, state, expected) tuples.
   Each non-blank, non-comment line has the form:
     LOGIC ; FORMULA ; STATE ; true/false
   where LOGIC is "K" (relational) or "PML" (probabilistic). *)
let parse_test_lines content =
  String.split_lines content
  |> List.filter_map ~f:(fun line ->
      let line = String.strip line in
      if
        String.is_empty line
        || String.is_prefix line ~prefix:"(*"
      then None
      else
        match
          String.split line ~on:';'
          |> List.map ~f:String.strip
        with
        | [ logic; formula; state; expected ] ->
            let expected_bool =
              match expected with
              | "true" -> true
              | "false" -> false
              | s -> failwith ("Invalid expected value:" ^ s)
            in
            Some (logic, formula, state, expected_bool)
        | _ -> failwith ("Invalid test line format: " ^ line))

(* Build a list of OUnit tests from a (model_path, test_path) pair.
   The logic field of the first test line determines which parser/checker to use. *)
let tests_from_pair (model_path, test_path) =
  let model_str = In_channel.read_all model_path in
  let test_lines =
    parse_test_lines (In_channel.read_all test_path)
  in
  let base =
    String.chop_suffix_exn
      (Filename.basename model_path)
      ~suffix:".model"
  in
  match test_lines with
  | [] -> []
  | (logic, _, _, _) :: _ ->
      let make_check =
        match logic with
        | "K" ->
            let model =
              Logics.Relational.parse_model model_str
            in
            fun formula_str point expected _ ->
              let formula =
                Logics.Relational.formula_of_ast
                  (Logics.Relational.parse_formula
                     formula_str)
              in
              let result =
                Checkers.Relational.model_check
                  ~verbose:false ~model
                  ~point:(State.of_string point)
                  ~formula
              in
              assert_equal ~printer:Bool.to_string expected
                result
        | "PML" ->
            let model =
              Logics.Probabilistic.parse_model model_str
            in
            fun formula_str point expected _ ->
              let formula =
                Logics.Probabilistic.formula_of_ast
                  (Logics.Probabilistic.parse_formula
                     formula_str)
              in
              let result =
                Checkers.Probabilistic.model_check
                  ~verbose:false ~model
                  ~point:(State.of_string point)
                  ~formula
              in
              assert_equal ~printer:Bool.to_string expected
                result
        | _ ->
            failwith
              (Printf.sprintf "Unknown logic: %S" logic)
      in
      List.mapi test_lines
        ~f:(fun i (_, formula, state, expected) ->
          Printf.sprintf "%s:%d" base (i + 1)
          >:: make_check formula state expected)

(* k12 (28 states) and prob_20states (20 states) are excluded: the naive
   checker computes the full powerset of states (2^n), which would OOM at
   2^28 ≈ 268M and hang at 2^20 ≈ 1M entries. The data files are kept for
   future use if a polynomial checker is added. *)
let test_pairs =
  [
    ("data/k1.model", "data/k1.test")
  ; ("data/k2.model", "data/k2.test")
  ; ("data/k3.model", "data/k3.test")
  ; ("data/k_game.model", "data/k_game.test")
  ; ("data/prob1.model", "data/prob1.test")
  ; ("data/prob_split.model", "data/prob_split.test")
  ]

let suite =
  "Model_checking"
  >::: List.concat_map test_pairs ~f:tests_from_pair

let () = run_test_tt_main suite
