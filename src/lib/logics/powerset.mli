open! Core
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

include
  Logic_intf.LOGIC_SPECIFICATION
    with module Model_spec = Model_parsers.Powerset
     and module Formula_spec = Formula_parsers.Powerset
