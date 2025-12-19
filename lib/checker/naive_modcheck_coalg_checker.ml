open! Core
open Naive_modcheck_coalg_logics

module Checkers = struct
  module Relational = Checker.Make (Logics.Relational)
end
