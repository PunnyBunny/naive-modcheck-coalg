open! Core
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

module Make
    (A : Logic_intf.LOGIC_SPECIFICATION)
    (B : Logic_intf.LOGIC_SPECIFICATION) :
  Logic_intf.LOGIC_SPECIFICATION
    with module Model_spec = Model_parsers.Composition.Make
                               (A.Model_spec)
                               (B.Model_spec)
     and module Formula_spec = Formula_parsers.Composition
                               .Make
                                 (A.Formula_spec)
                                 (B.Formula_spec)
