open Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics
open Cmdliner
module Formula = Logics.Relational.Relational_formula

let var i = Var.of_string ("x" ^ Int.to_string i)
let atom i = Ap.of_string ("p" ^ Int.to_string i)
let even = Ap.of_string "even"
let odd = Ap.of_string "odd"

let branch i =
  let xi = Formula.Var (var i) in
  let even_step =
    Formula.And
      (Formula.Ap even, Formula.Modal (Diamond xi))
  in
  let odd_step =
    Formula.And (Formula.Ap odd, Formula.Modal (Box xi))
  in
  Formula.And
    (Formula.Ap (atom i), Formula.Or (even_step, odd_step))

let disjunction_upto k =
  List.range 0 (k + 1) |> List.map ~f:branch |> function
  | [] -> Formula.False
  | hd :: tl ->
      List.fold tl ~init:hd ~f:(fun acc f ->
          Formula.Or (acc, f))

let build_formula k =
  let body = disjunction_upto k in
  List.range 0 (k + 1)
  |> List.fold ~init:body ~f:(fun acc i ->
      if i % 2 = 0 then Formula.Nu (var i, acc)
      else Formula.Mu (var i, acc))

let run max_priority outdir =
  if max_priority < 0 then
    raise_s [%message "max priority must be non-negative"];
  let content =
    build_formula max_priority |> Formula.pretty_print
  in
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
