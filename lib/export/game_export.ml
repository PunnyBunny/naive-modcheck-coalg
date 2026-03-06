open! Core

(** Export a solved parity game as JSON for the web viewer. *)

let export_json
    ~(game_data : Naive_modcheck_coalg_checker.Checker_intf.game_data) ~model_src
    ~formula_src ~logic ~point : Yojson.Safe.t =
  let nodes =
    List.init game_data.num_nodes ~f:(fun i ->
        `Assoc
          [
            ("id", `Int i);
            ("label", `String game_data.node_labels.(i));
            ("owner", `String game_data.node_owners.(i));
            ("priority", `Int game_data.node_priorities.(i));
            ("winner", `String game_data.winners.(i));
            ("isStart", `Bool (i = game_data.starting_node));
            ("isModal", `Bool game_data.node_is_modal.(i));
            ( "states",
              `List
                (List.map game_data.node_states.(i) ~f:(fun s -> `String s)) );
          ])
  in
  let edges =
    List.concat_map (List.init game_data.num_nodes ~f:Fn.id) ~f:(fun src ->
        List.map game_data.node_successors.(src) ~f:(fun tgt ->
            let is_strategy =
              match game_data.strategy.(src) with
              | Some s -> s = tgt
              | None -> false
            in
            `Assoc
              [
                ("source", `Int src);
                ("target", `Int tgt);
                ("isStrategy", `Bool is_strategy);
              ]))
  in
  `Assoc
    [
      ( "metadata",
        `Assoc
          [
            ("logic", `String logic);
            ("formula", `String formula_src);
            ("model", `String model_src);
            ("point", `String point);
            ("result", `Bool game_data.result);
          ] );
      ("nodes", `List nodes);
      ("edges", `List edges);
    ]

let to_file ~path json =
  let content = Yojson.Safe.pretty_to_string json in
  Out_channel.write_all path ~data:content
