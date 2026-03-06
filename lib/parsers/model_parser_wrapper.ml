open! Core
module I = Model_parser.MenhirInterpreter
(* name is usually MenhirInterpreter *)

let parse_model input (type t)
    (start : Lexing.position -> t I.checkpoint) : t =
  let lexbuf = Lexing.from_string input in
  let checkpoint = start lexbuf.Lexing.lex_curr_p in

  let rec loop (checkpoint : t I.checkpoint) =
    match checkpoint with
    | InputNeeded _ ->
        let token = Model_lexer.lex lexbuf in
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
        let message = Model_parser_messages.message state in
        failwith
          (Printf.sprintf
             "Parse error at line %d, column %d: %s, \
              state=%d"
             line col message state)
  in
  loop checkpoint

let parse_relational_model (input : string) =
  parse_model input
    Model_parser.Incremental.relational_model

let parse_graded_model (input : string) =
  parse_model input Model_parser.Incremental.graded_model

let parse_probabilistic_model (input : string) =
  parse_model input
    Model_parser.Incremental.probabilistic_model

let parse_monotone_model (input : string) =
  parse_model input Model_parser.Incremental.monotone_model
