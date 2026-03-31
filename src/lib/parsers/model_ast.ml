open! Core
open Naive_modcheck_coalg_common

module Relational_ast = Model_intf.Make (struct
  type t = State.t list [@@deriving sexp]

  let to_string states =
    let inner = String.concat ~sep:", " (List.map ~f:State.to_string states) in
    {%string|{%{inner}}|}
end)

module Graded_ast = Model_intf.Make (struct
  type t = (State.t, int) Hashtbl.Poly.t [@@deriving sexp]

  let to_string tbl =
    let entries =
      Hashtbl.Poly.fold tbl ~init:[] ~f:(fun ~key:state ~data:n acc ->
        let state = State.to_string state in
        let n = Int.to_string n in
        {%string|%{state}: %{n}|} :: acc)
    in
    let inner = String.concat ~sep:", " entries in
    {%string|[%{inner}]|}
end)

module Probabilistic_ast = struct
  type frac = Frac.t [@@deriving sexp]

  include Model_intf.Make (struct
    type t = (State.t, frac) Hashtbl.Poly.t
    [@@deriving sexp]

    let to_string tbl =
      let entries =
        Hashtbl.Poly.fold tbl ~init:[] ~f:(fun ~key:state ~data:f acc ->
          let state = State.to_string state in
          let f = Frac.to_string f in
          {%string|%{state}: %{f}|} :: acc)
      in
      let inner = String.concat ~sep:", " entries in
      {%string|[%{inner}]|}
  end)
end

module Monotone_ast = Model_intf.Make (struct
  type t = State.t list list [@@deriving sexp]

  let to_string ll =
    let pp_list l =
      let inner = String.concat ~sep:", " (List.map ~f:State.to_string l) in
      {%string|{%{inner}}|}
    in
    let inner = String.concat ~sep:", " (List.map ~f:pp_list ll) in
    {%string|{%{inner}}|}
end)
