open Core
open Cmdliner

(* TODO: change into Formula constructors not strings *)

let branch i =
  Printf.sprintf
    "(p%d && ((even && <>x%d) || (odd && []x%d)))" i i i

let disjunction_upto k =
  List.range 0 (k + 1) |> List.map ~f:branch |> function
  | [] -> "false"
  | hd :: tl ->
      List.fold tl ~init:hd ~f:(fun acc f ->
          Printf.sprintf "(%s || %s)" acc f)

let build_formula k =
  let body = disjunction_upto k in
  List.range 0 (k + 1)
  |> List.fold ~init:body ~f:(fun acc i ->
      if i % 2 = 0 then Printf.sprintf "nu x%d . (%s)" i acc
      else Printf.sprintf "mu x%d . (%s)" i acc)

let run max_priority outdir =
  if max_priority < 0 then
    raise_s [%message "max priority must be non-negative"];
  let content = build_formula max_priority in
  match outdir with
  | None -> print_endline content
  | Some dir ->
      let path =
        Filename.concat dir
          ("formula_" ^ Int.to_string max_priority ^ ".mcf")
      in
      let oc = Out_channel.create path in
      Fun.protect
        ~finally:(fun () -> Out_channel.close oc)
        (fun () -> Printf.fprintf oc "%s\n" content)

let max_priority_arg =
  let doc =
    "Maximum priority $(docv) used in the formula."
  in
  Arg.(
    required
    & pos 0 (some int) None
    & info [] ~docv:"MAX_PRIORITY" ~doc)

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
    "Generate alternating fixpoint benchmark formulas."
  in
  let info = Cmd.info "formula_gen" ~doc in
  Cmd.v info
    Term.(const run $ max_priority_arg $ outdir_arg)

let () = exit (Cmd.eval cmd)
