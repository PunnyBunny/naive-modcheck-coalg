open! Core

module Identity = struct
  type 'a t = 'a [@@deriving sexp]

  let to_string state ~to_string_parent =
    to_string_parent state

  let parser p = p
end
