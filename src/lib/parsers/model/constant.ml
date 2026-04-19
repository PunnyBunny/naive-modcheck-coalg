open! Core
open Naive_modcheck_coalg_parsers_common.Lexer

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string, equal]

  val parser : t Angstrom.t
  val default : unit -> t
end

module Make_list (A : sig
  type t [@@deriving sexp, to_string, equal]

  val parser : t Angstrom.t
end) =
struct
  type t = A.t list [@@deriving sexp, equal]

  let to_string lst =
    let inner =
      lst
      |> List.map ~f:A.to_string
      |> String.concat ~sep:", "
    in
    Printf.sprintf "{%s}" inner

  let parser =
    let open Angstrom in
    kw "{" *> sep_by (kw ",") A.parser <* kw "}"

  let default () = []
end

module Make (A : CONSTANT_SPEC) = struct
  type 'a t = A.t [@@deriving sexp]

  let to_string state ~to_string_inner:_ = A.to_string state
  let parser ~model_inner:_ = A.parser
  let default () = A.default ()
end
