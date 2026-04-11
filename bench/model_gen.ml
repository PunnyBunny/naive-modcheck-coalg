open Core
open Cmdliner

(* TODO: change into Model constructors not strings *)

let generators =
  [
    "randomgame"
  ; "laddergame"
  ; "towersofhanoi"
  ; "langincl"
    (* TODO: add more generators *)
    (* "cliquegame"; "modelcheckerladder"; "recursiveladder"; *)
    (* "recursivedullgame"; "steadygame"; "jurdzinskigame"; *)
    (* "elevatorvergm"; "roadworksvergm"; "clusteredrg" *)
  ]

let register_all () =
  Randomgame.register ();
  Laddergame.register ();
  Towersofhanoi.register ();
  Langincl.register ()

let default_args gen n =
  let s = string_of_int n in
  match gen with
  | "randomgame" ->
      let p = string_of_int (max 2 (n / 5)) in
      let hi = string_of_int (max 2 (min n 5)) in
      [| s; p; "1"; hi |]
  | "laddergame"
  | "towersofhanoi"
  | "langincl" ->
      [| s |]
  (* TODO: uncomment when generators are added *)
  (* | "cliquegame" -> [| s; "self" |] *)
  (* | "modelcheckerladder" | "recursiveladder" *)
  (* | "recursivedullgame" | "roadworksvergm" *)
  (* | "elevatorvergm" -> [| s |] *)
  (* | "steadygame" -> [| s; "1"; hi; "1"; hi |] *)
  (* | "jurdzinskigame" -> [| s; s |] *)
  (* | "clusteredrg" -> [| s; p; "1"; hi; depth; "2"; bhi; "1"; chi |] *)
  | g -> failwith ("Unknown generator: " ^ g)

type format = Pgsolver | Model

let print_pgsolver oc pg = Paritygame.(output_game oc pg)

let print_model oc pg =
  let open Paritygame in
  let entries = ref [] in
  pg_iterate
    (fun node (prio, owner, succs, _preds, _desc) ->
      let owner_ap =
        if Poly.(owner = Paritygame.plr_Even) then "even"
        else "odd"
      in
      let aps = [%string "%{owner_ap}, p%{prio#Int}"] in
      let succ_states =
        ns_fold
          (fun acc s -> [%string "s%{s#Int}"] :: acc)
          [] succs
        |> List.rev
      in
      let succs_str = String.concat ~sep:", " succ_states in
      let entry =
        [%string
          "s%{node#Int}: ({%{aps}}, [{}: {%{succs_str}}])"]
      in
      entries := entry :: !entries)
    pg;
  let body =
    !entries |> List.rev |> String.concat ~sep:", "
  in
  Printf.fprintf oc "[%s]\n" body

let run gen size format outdir =
  register_all ();
  let args = default_args gen size in
  let generator, _ = Generatorregistry.find_generator gen in
  let pg = generator args in
  let ext =
    match format with
    | Pgsolver -> ".gm"
    | Model -> ".model"
  in
  let printer =
    match format with
    | Pgsolver -> print_pgsolver
    | Model -> print_model
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
     $(b,model) for this repo's model format."
  in
  Arg.(
    value
    & opt
        (enum [ ("pgsolver", Pgsolver); ("model", Model) ])
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
    "Generate parity games using PGSolver generators"
  in
  let info = Cmd.info "model_gen" ~doc in
  Cmd.v info
    Term.(
      const run $ gen_arg $ size_arg $ format_arg
      $ outdir_arg)

let () = exit (Cmd.eval cmd)
