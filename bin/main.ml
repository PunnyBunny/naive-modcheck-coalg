open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_checker
open Naive_modcheck_coalg_logics

(* ── Output helpers ────────────────────────────────────────────────────────── *)

type check_result = { result : bool; state : string; logic : string }
[@@deriving sexp]

let print_result ~logic ~point ~result =
  { result; state = State.to_string point; logic }
  |> sexp_of_check_result |> Sexp.to_string_hum |> print_endline

let print_error msg =
  [%sexp { error : string = msg }] |> Sexp.to_string_hum |> prerr_endline

(* ── I/O helpers ──────────────────────────────────────────────────────────── *)

(** Resolve an inline string vs. a file path, requiring exactly one. *)
let read_source ~label ~inline ~file =
  match (inline, file) with
  | Some s, None -> Ok s
  | None, Some p -> (
      try Ok (In_channel.read_all p)
      with Sys_error e -> Error (sprintf "Cannot read %s file: %s" label e))
  | None, None -> Error (sprintf "Provide either --%s or --%s-file" label label)
  | Some _, Some _ ->
      Error (sprintf "Use either --%s or --%s-file, not both" label label)

(* ── Model-checking dispatch ──────────────────────────────────────────────── *)

let run_relational ~verbose ~model_src ~formula_src ~point_str =
  let point = State.of_string point_str in
  let model =
    try Logics.Relational.parse_model model_src
    with exn ->
      print_error
        (sprintf "Failed to parse relational model: %s" (Exn.to_string exn));
      exit 1
  in
  let formula_ast =
    try Logics.Relational.parse_formula formula_src
    with exn ->
      print_error
        (sprintf "Failed to parse relational formula: %s" (Exn.to_string exn));
      exit 1
  in
  let formula = Logics.Relational.formula_of_ast formula_ast in
  let result =
    Checkers.Relational.model_check ~verbose ~model ~point ~formula
  in
  print_result ~logic:"relational" ~point ~result

let run_probabilistic ~verbose ~model_src ~formula_src ~point_str =
  let point = State.of_string point_str in
  let model =
    try Logics.Probabilistic.parse_model model_src
    with exn ->
      print_error
        (sprintf "Failed to parse probabilistic model: %s" (Exn.to_string exn));
      exit 1
  in
  let formula_ast =
    try Logics.Probabilistic.parse_formula formula_src
    with exn ->
      print_error
        (sprintf "Failed to parse probabilistic formula: %s" (Exn.to_string exn));
      exit 1
  in
  let formula = Logics.Probabilistic.formula_of_ast formula_ast in
  let result =
    Checkers.Probabilistic.model_check ~verbose ~model ~point ~formula
  in
  print_result ~logic:"probabilistic" ~point ~result

(* ── CLI definition ───────────────────────────────────────────────────────── *)

let check_cmd =
  let open Cmdliner in
  (* Arguments *)
  let logic =
    let doc =
      "Logic to use for model checking. Must be $(b,relational) or \
       $(b,probabilistic)."
    in
    Arg.(
      required & opt (some string) None & info [ "logic" ] ~docv:"LOGIC" ~doc)
  in
  let model_inline =
    let doc =
      "Model supplied as an inline string. Mutually exclusive with \
       $(b,--model-file)."
    in
    Arg.(value & opt (some string) None & info [ "model" ] ~docv:"STRING" ~doc)
  in
  let model_file =
    let doc =
      "Path to a file containing the model. Mutually exclusive with \
       $(b,--model)."
    in
    Arg.(value & opt (some file) None & info [ "model-file" ] ~docv:"PATH" ~doc)
  in
  let formula_inline =
    let doc =
      "Formula supplied as an inline string. Mutually exclusive with \
       $(b,--formula-file)."
    in
    Arg.(
      value & opt (some string) None & info [ "formula" ] ~docv:"STRING" ~doc)
  in
  let formula_file =
    let doc =
      "Path to a file containing the formula. Mutually exclusive with \
       $(b,--formula)."
    in
    Arg.(
      value & opt (some file) None & info [ "formula-file" ] ~docv:"PATH" ~doc)
  in
  let point =
    let doc = "Initial state to check the formula at." in
    Arg.(
      required
      & opt (some string) None
      & info [ "point"; "state" ] ~docv:"STATE" ~doc)
  in
  let verbose =
    let doc = "Print verbose parity-game solving output." in
    Arg.(value & flag & info [ "verbose"; "v" ] ~doc)
  in

  (* Main action *)
  let action logic model_inline model_file formula_inline formula_file point
      verbose =
    let model_src =
      match
        read_source ~label:"model" ~inline:model_inline ~file:model_file
      with
      | Ok s -> s
      | Error e ->
          print_error e;
          exit 1
    in
    let formula_src =
      match
        read_source ~label:"formula" ~inline:formula_inline ~file:formula_file
      with
      | Ok s -> s
      | Error e ->
          print_error e;
          exit 1
    in
    match logic with
    | "relational" ->
        run_relational ~verbose ~model_src ~formula_src ~point_str:point
    | "probabilistic" ->
        run_probabilistic ~verbose ~model_src ~formula_src ~point_str:point
    | other ->
        print_error
          (sprintf "Unknown logic %S. Choose 'relational' or 'probabilistic'."
             other);
        exit 1
  in

  let man_description =
    {|Checks whether a state in a labeled transition system satisfies
a mu-calculus formula using coalgebraic parity-game reduction.|}
  in
  let rel_formula_examples =
    {|  nu X.(mu Y.((p && <> Y) || (q && [] X)))
  <a> true
  [a] false|}
  in
  let rel_model_example =
    {|  [s0: ({p, q}, [a: {s1, s2}]), s1: ({q}, [a: {s0}])]|}
  in
  let prob_formula_examples = {|  <1/2 a> true
  [3/4 {}] false|} in
  let prob_model_example = {|  [s0: ({p}, [a: [s1: 1/2, s2: 1/2]])]|} in
  let example_relational =
    {|  dune exec naive-modcheck-coalg -- \
    --logic relational \
    --model '[x:({p},[{}:{x,y}]),y:({q},[{}:{x,y}])]' \
    --formula '<> true' \
    --point x|}
  in
  let example_probabilistic =
    {|  dune exec naive-modcheck-coalg -- \
    --logic probabilistic \
    --model-file model.txt \
    --formula-file formula.txt \
    --point s0|}
  in
  let info =
    Cmd.info "naive-modcheck-coalg" ~version:"0.1.0"
      ~doc:"Coalgebraic modal-logic model checker"
      ~man:
        [
          `S Manpage.s_description;
          `P man_description;
          `S "INPUT SYNTAX";
          `P "$(b,Relational formula) examples:";
          `Pre rel_formula_examples;
          `P "$(b,Relational model) format:";
          `Pre rel_model_example;
          `P "$(b,Probabilistic formula) examples:";
          `Pre prob_formula_examples;
          `P "$(b,Probabilistic model) format:";
          `Pre prob_model_example;
          `S Manpage.s_examples;
          `P "Check a relational formula from inline strings:";
          `Pre example_relational;
          `P "Check a probabilistic formula from files:";
          `Pre example_probabilistic;
        ]
  in

  Cmd.v info
    Term.(
      const action $ logic $ model_inline $ model_file $ formula_inline
      $ formula_file $ point $ verbose)

let () = exit (Cmdliner.Cmd.eval check_cmd)
