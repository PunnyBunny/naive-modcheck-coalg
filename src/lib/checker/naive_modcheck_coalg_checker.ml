open! Core
open Naive_modcheck_coalg_logics

module Checkers = struct
  module Relational = Checker.Make (Logics.Relational)
  module Probabilistic = Checker.Make (Logics.Probabilistic)
end

module Checker_intf = Checker_intf
