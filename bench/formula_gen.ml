open Core
open Cmdliner

(** Alternating-fixpoint benchmark formula for parity-game enrichment, per §4 of
    the COOL-MC paper (arxiv 2311.01315). Shape:

      fp_d x_d . ... . fp_0 x_0 . \/_{i=0..d} (p_i & ((eloise & <>_i x_i) |
                                                     (abelard & []_i x_i)))

    where fp alternates nu (even i) and mu (odd i), and <>_i / []_i instantiate
    per (logic, target-tool) below. *)

type logic = Relational | Probabilistic

type target = Naive | Cool

(** The shared structure is tiny, so we keep it as two printer-specific builders
    rather than a full AST. Each builder returns the formula string directly. *)

let var_name i = [%string "x%{i#Int}"]

let prop_name i = [%string "p%{i#Int}"]

let fp_binder ~i =
  if i mod 2 = 0 then [%string "nu %{var_name i}"]
  else [%string "mu %{var_name i}"]

(* ── Per-(logic, target) branch formatters ──────────────────────────────── *)

(** For priority [i], returns the branch

      (p_i & ((eloise & diamond x_i) | (abelard & box x_i)))

    where diamond/box depend on the target tool and logic. *)
let branch ~logic ~target ~i =
  let pi = prop_name i in
  let xi = var_name i in
  let diamond, box, ap_eloise, ap_abelard, prop =
    match (target, logic) with
    | Naive, Relational ->
        ( [%string "[@2][a]<> %{xi}"]
        , [%string "[@2][a][] %{xi}"]
        , "[@1]eloise"
        , "[@1]abelard"
        , [%string "[@1]%{pi}"] )
    | Naive, Probabilistic ->
        ( [%string "[@2][a][>=1/2] %{xi}"]
        , [%string "[@2][a][>1/2] %{xi}"]
        , "[@1]eloise"
        , "[@1]abelard"
        , [%string "[@1]%{pi}"] )
    | Cool, Relational ->
        ( [%string "<a>(%{xi})"]
        , [%string "[a](%{xi})"]
        , "eloise"
        , "abelard"
        , pi )
    | Cool, Probabilistic ->
        ( [%string "{>=50/100}(%{xi})"]
        , [%string "{<50/100}~(%{xi})"]
        , "eloise"
        , "abelard"
        , pi )
  in
  [%string
    "(%{prop} & ((%{ap_eloise} & %{diamond}) | (%{ap_abelard} & %{box})))"]

(** Disjunction of branches for priorities 0..d. *)
let disjunction_upto ~logic ~target d =
  List.range 0 (d + 1)
  |> List.map ~f:(fun i -> branch ~logic ~target ~i)
  |> function
  | [] -> "false"
  | hd :: tl ->
      List.fold tl ~init:hd ~f:(fun acc f ->
          [%string "(%{acc} | %{f})"])

(** Wraps the body in alternating fixpoints x_0 (innermost) .. x_d (outermost). *)
let build_formula ~logic ~target ~max_priority =
  let body = disjunction_upto ~logic ~target max_priority in
  List.range 0 (max_priority + 1)
  |> List.fold ~init:body ~f:(fun acc i ->
         [%string "%{fp_binder ~i} . (%{acc})"])

(* ── CLI ───────────────────────────────────────────────────────────────── *)

let filename ~logic ~target ~max_priority =
  let logic_tag =
    match logic with Relational -> "rel" | Probabilistic -> "prob"
  in
  let target_tag =
    match target with Naive -> "naive" | Cool -> "cool"
  in
  [%string "formula_%{logic_tag}_%{target_tag}_%{max_priority#Int}.mcf"]

let run logic target max_priority outdir =
  if max_priority < 0 then
    raise_s [%message "max priority must be non-negative"];
  let content = build_formula ~logic ~target ~max_priority in
  match outdir with
  | None -> print_endline content
  | Some dir ->
      let path =
        Filename.concat dir (filename ~logic ~target ~max_priority)
      in
      let oc = Out_channel.create path in
      Fun.protect
        ~finally:(fun () -> Out_channel.close oc)
        (fun () -> Printf.fprintf oc "%s\n" content)

let max_priority_arg =
  let doc = "Maximum priority used in the formula." in
  Arg.(
    required
    & pos 0 (some int) None
    & info [] ~docv:"MAX_PRIORITY" ~doc)

let logic_arg =
  let doc =
    "Logic the formula targets. $(b,relational) emits the K \
     (mu-calculus over powerset) formula; $(b,probabilistic) \
     emits the PML (reactive, threshold 1/2) formula."
  in
  Arg.(
    value
    & opt
        (enum
           [
             ("relational", Relational); ("probabilistic", Probabilistic)
           ])
        Relational
    & info [ "logic" ] ~docv:"LOGIC" ~doc)

let target_arg =
  let doc =
    "Formula flavour. $(b,naive) emits syntax accepted by \
     naive-modcheck-coalg (compositional $(b,[@1]) / $(b,[@2]) \
     slots). $(b,cool) emits syntax accepted by cool-coalg \
     (bare APs, $(b,<a>)/$(b,[a]) for relational, \
     $(b,{>=50/100})/$(b,{<50/100}) for PML)."
  in
  Arg.(
    value
    & opt (enum [ ("naive", Naive); ("cool", Cool) ]) Naive
    & info [ "target" ] ~docv:"TARGET" ~doc)

let outdir_arg =
  let doc =
    "Output directory. If omitted, prints to stdout."
  in
  Arg.(
    value
    & opt (some dir) None
    & info [ "o" ] ~docv:"DIR" ~doc)

let cmd =
  let doc =
    "Generate alternating-fixpoint benchmark formulas from \
     parity-game enrichments, in the syntax of the requested \
     target tool."
  in
  let info = Cmd.info "formula_gen" ~doc in
  Cmd.v info
    Term.(
      const run $ logic_arg $ target_arg $ max_priority_arg
      $ outdir_arg)

let () = exit (Cmd.eval cmd)
