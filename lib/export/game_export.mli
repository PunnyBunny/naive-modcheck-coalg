(** Export a solved parity game as JSON for the web viewer. *)

val export_json :
  game_data:Naive_modcheck_coalg_checker.Checker_intf.game_data ->
  model_src:string ->
  formula_src:string ->
  logic:string ->
  point:string ->
  Yojson.Safe.t

val to_file : path:string -> Yojson.Safe.t -> unit
