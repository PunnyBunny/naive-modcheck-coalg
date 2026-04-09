open! Core
open Naive_modcheck_coalg_common
module Ap : Constant.CONSTANT_SPEC with type t = Ap.t

module Actions :
  Exp_by_set.EXP_BY_SET_SPEC with type t = Action.t

module Formula_spec : Formula_parser.SPEC
