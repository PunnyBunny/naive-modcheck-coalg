open! Core
module I = Formula_parser.MenhirInterpreter

let parse_formula input (type t)
    (start : Lexing.position -> t I.checkpoint) : t =
  let lexbuf = Lexing.from_string input in
  let checkpoint = start lexbuf.Lexing.lex_curr_p in

  let rec loop (checkpoint : t I.checkpoint) =
    match checkpoint with
    | InputNeeded _ ->
        let token = Formula_lexer.lex lexbuf in
        let startp = lexbuf.lex_start_p
        and endp = lexbuf.lex_curr_p in
        let checkpoint =
          I.offer checkpoint (token, startp, endp)
        in
        loop checkpoint
    | Shifting _
    | AboutToReduce _ ->
        let checkpoint = I.resume checkpoint in
        loop checkpoint
    | Accepted v -> v
    | Rejected -> failwith "Parse error: rejected input"
    | HandlingError env ->
        let loc = I.positions env |> fst in
        let line = loc.Lexing.pos_lnum in
        let col =
          loc.Lexing.pos_cnum - loc.Lexing.pos_bol
        in
        let state = I.current_state_number env in
        failwith
          (Printf.sprintf
             "Parse error at line %d, column %d, state=%d"
             line col state)
  in
  loop checkpoint

let parse_relational_formula (input : string) =
  parse_formula input
    Formula_parser.Incremental.relational_formula

let parse_probabilistic_formula (input : string) =
  parse_formula input
    Formula_parser.Incremental.probabilistic_formula
