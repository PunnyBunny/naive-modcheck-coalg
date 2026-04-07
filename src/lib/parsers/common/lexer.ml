open! Core
open Angstrom

let is_whitespace = function
  | ' '
  | '\t'
  | '\n'
  | '\r' ->
      true
  | _ -> false

let is_alnum_underscore = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_' ->
      true
  | _ -> false

let skip_comment =
  string "//" *> skip_while (fun c -> Char.(c <> '\n'))

let spacing =
  skip_many
    (take_while1 is_whitespace *> return () <|> skip_comment)

let tok p = p <* spacing
let kw s = tok (string s)

let word =
  tok
    (lift2
       (fun c rest -> String.of_char c ^ rest)
       (satisfy Char.is_alpha)
       (take_while is_alnum_underscore))

let integer =
  tok (take_while1 Char.is_digit) >>| int_of_string
