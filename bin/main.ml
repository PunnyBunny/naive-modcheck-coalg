open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_export
open Naive_modcheck_coalg_registry
open Cmdliner

(* ── Output helpers ────────────────────────────────────────────────────────── *)

type check_result = { result : bool; state : string; logic : string }
[@@deriving sexp]

let print_result ~logic ~point ~result =
  { result; state = State.to_string point; logic }
  |> sexp_of_check_result |> Sexp.to_string_hum |> print_endline

let print_error msg =
  [%sexp { error : string = msg }] |> Sexp.to_string_hum |> prerr_endline

(* ── I/O helpers ──────────────────────────────────────────────────────────── *)

let default_visualiser_filename ~logic =
  let time =
    Time_ns.now () |> Time_ns.to_filename_string ~zone:Time_float.Zone.utc
  in
  sprintf "parity-game-%s-%s.json" logic time

let resolve_visualise_path ~logic path =
  if Stdlib.Sys.is_directory path then
    Filename.concat path (default_visualiser_filename ~logic)
  else path

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

let run_logic ~(packed : packed_logic) ~verbose ~visualise ~model_src
    ~formula_src ~point_str =
  let logic_name = name packed in
  let point = State.of_string point_str in
  let game_data =
    try run packed ~verbose ~model_src ~formula_src ~point
    with exn ->
      print_error
        (sprintf "Failed to run %s model checking: %s" logic_name
           (Exn.to_string exn));
      exit 1
  in
  (match visualise with
  | Some path ->
      let path = resolve_visualise_path ~logic:logic_name path in
      let json =
        Game_export.export_json ~game_data ~model_src ~formula_src
          ~logic:logic_name ~point:point_str
      in
      Game_export.to_file ~path json
  | None -> ());
  print_result ~logic:logic_name ~point ~result:game_data.result

(* ── CLI definition ───────────────────────────────────────────────────────── *)

let check_cmd =
  (* Arguments *)
  let logic =
    let known = known_names () |> String.concat ~sep:", " in
    let doc =
      sprintf "Logic to use for model checking. Must be one of: %s." known
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
  let visualise =
    let doc =
      "Export the solved parity game as JSON to $(docv) for use with the web \
       viewer. If $(docv) is a directory, writes to parity-game.json inside \
       it."
    in
    Arg.(
      value
      & opt (some string) None
      & info [ "visualise"; "V" ] ~docv:"PATH" ~doc)
  in

  (* Main action *)
  let action logic model_inline model_file formula_inline formula_file point
      verbose visualise =
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
    match find_logic logic with
    | Some packed ->
        run_logic ~packed ~verbose ~visualise ~model_src ~formula_src
          ~point_str:point
    | None ->
        let known = known_names () |> String.concat ~sep:", " in
        print_error (sprintf "Unknown logic %S. Choose one of: %s." logic known);
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
      $ formula_file $ point $ verbose $ visualise)

let main () = Cmdliner.Cmd.eval check_cmd
let () = exit (main ())
