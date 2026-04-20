open Core
open Cmdliner

let generators =
  [
    "cliquegame"
  ; "laddergame"
  ; "jurdzinskigame"
  ; "towersofhanoi"
  ; "langincl"
  ]

let register_all () =
  Cliquegame.register ();
  Laddergame.register ();
  Jurdzinskigame.register ();
  Towersofhanoi.register ();
  Langincl.register ()

(** Maps the benchmark's single-integer [size] to the argv each PGSolver
    generator expects. Conventions follow COOL's parity-mod benchmark
    (cool/benchmarks/modcheck/parity-mod/bench.py): jurdzinski and langincl
    take two args (h, w / n, n), clique takes n+1. *)
let default_args gen n =
  let s = string_of_int n in
  match gen with
  | "cliquegame" -> [| string_of_int (n + 1) |]
  | "laddergame"
  | "towersofhanoi" ->
      [| s |]
  | "jurdzinskigame"
  | "langincl" ->
      [| s; s |]
  | g -> failwith ("Unknown generator: " ^ g)

type format = Pgsolver | Rel_model | Prob_model

let action_label = "a"

let print_pgsolver oc pg = Paritygame.(output_game oc pg)

(** Collects (node, priority, owner, successors) in stable (node-ascending) order. *)
let pg_entries pg =
  let open Paritygame in
  let entries = ref [] in
  pg_iterate
    (fun node (prio, owner, succs, _preds, _desc) ->
      entries := (node, prio, owner, succs) :: !entries)
    pg;
  List.sort !entries ~compare:(fun (a, _, _, _) (b, _, _, _) ->
      Int.compare a b)

let state_name n = [%string "s%{n#Int}"]

let owner_ap owner =
  if Poly.(owner = Paritygame.plr_Even) then "eloise"
  else "abelard"

let ap_set prio owner =
  [%string "{%{owner_ap owner}, p%{prio#Int}}"]

let successor_names succs =
  let open Paritygame in
  ns_fold (fun acc s -> state_name s :: acc) [] succs |> List.rev

let print_rel_entry (node, prio, owner, succs) =
  let succs_str = String.concat ~sep:", " (successor_names succs) in
  [%string
    "%{state_name node}: (%{ap_set prio owner}, \
     [%{action_label}: {%{succs_str}}])"]

let print_prob_entry (node, prio, owner, succs) =
  let names = successor_names succs in
  let n = List.length names in
  let body =
    List.map names ~f:(fun s -> [%string "%{s}: 1/%{n#Int}"])
    |> String.concat ~sep:", "
  in
  [%string
    "%{state_name node}: (%{ap_set prio owner}, \
     [%{action_label}: [%{body}]])"]

let print_model oc pg ~entry_printer =
  let entries = pg_entries pg in
  let body =
    entries |> List.map ~f:entry_printer |> String.concat ~sep:", "
  in
  Printf.fprintf oc "[%s]\n" body

let run gen size format outdir =
  register_all ();
  let args = default_args gen size in
  let generator, _ = Generatorregistry.find_generator gen in
  let pg = generator args in
  let ext, printer =
    match format with
    | Pgsolver -> (".gm", print_pgsolver)
    | Rel_model ->
        (".model", fun oc pg -> print_model oc pg ~entry_printer:print_rel_entry)
    | Prob_model ->
        ( ".prob.model"
        , fun oc pg -> print_model oc pg ~entry_printer:print_prob_entry )
  in
  match outdir with
  | None -> printer stdout pg
  | Some dir ->
      let path =
        Filename.concat dir
          (gen ^ "_" ^ string_of_int size ^ ext)
      in
      let oc = Out_channel.create path in
      Fun.protect
        ~finally:(fun () -> Out_channel.close oc)
        (fun () -> printer oc pg)

let gen_arg =
  let doc = "Name of the parity game generator." in
  let generators_enum =
    List.map ~f:(fun g -> (g, g)) generators
  in
  Arg.(
    required
    & pos 0 (some (enum generators_enum)) None
    & info [] ~docv:"GENERATOR" ~doc)

let size_arg =
  let doc = "Size parameter for the generator." in
  Arg.(
    required
    & pos 1 (some int) None
    & info [] ~docv:"SIZE" ~doc)

let format_arg =
  let doc =
    "Output format. $(b,pgsolver) for PGSolver .gm format, \
     $(b,rel-model) for the relational model format (sets of \
     successors), $(b,prob-model) for the probabilistic model \
     format (uniform distributions over successors)."
  in
  Arg.(
    value
    & opt
        (enum
           [
             ("pgsolver", Pgsolver)
           ; ("rel-model", Rel_model)
           ; ("prob-model", Prob_model)
           ])
        Pgsolver
    & info [ "format" ] ~docv:"FORMAT" ~doc)

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
    "Generate parity games using PGSolver generators, either \
     as .gm files or enriched into relational/probabilistic \
     mu-calculus model files."
  in
  let info = Cmd.info "model_gen" ~doc in
  Cmd.v info
    Term.(
      const run $ gen_arg $ size_arg $ format_arg
      $ outdir_arg)

let () = exit (Cmd.eval cmd)
