open! Core

include
  Logic.Make
    (Product.Make
       (Constant_list.Make
          (Specs.Ap))
          (Exp_by_set.Make (Specs.Actions) (Powerset)))
