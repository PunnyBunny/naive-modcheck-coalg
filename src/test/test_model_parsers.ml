open! Core
open OUnit2
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_model.Model_parsers
open Naive_modcheck_coalg_logics
module Ap_list_spec = Constant.Make_list (Specs.Ap)

let test_parser (module M : Model.S) ~expected ~model =
  assert_equal ~cmp:Sexp.equal ~printer:Sexp.to_string_hum
    (Sexp.of_string expected)
    (M.parse_model model |> [%sexp_of: M.t])

module Constant_test = struct
  module M =
    Model_parser.Make
      (Constant.Make (Constant.Make_list (Specs.Ap)))

  let test = test_parser (module M)

  let test_parsing _ =
    test ~model:"[x: {p}]" ~expected:"((x (p)))";
    test ~model:"[x: {p, q}]" ~expected:"((x (p q)))";
    test ~model:"[x: {}]" ~expected:"((x ()))";
    test ~model:"[]" ~expected:"()"
end

module Powerset_test = struct
  module M = Model_parser.Make (Powerset)

  let test = test_parser (module M)

  let test_parsing _ =
    test ~model:"[x: {y, z}]" ~expected:"((x (y z)))";
    test ~model:"[x: {}]" ~expected:"((x ()))"
end

module Distribution_test = struct
  module M = Model_parser.Make (Distribution)

  let test = test_parser (module M)

  let test_parsing _ =
    test ~model:"[x: [y: 1/2]]"
      ~expected:"((x ((y (1 2)))))";
    test ~model:"[x: [y: 3/4]]"
      ~expected:"((x ((y (3 4)))))"
end

module Exp_pow_test = struct
  module M =
    Model_parser.Make
      (Exp_by_set.Make (Specs.Actions) (Powerset))

  let test = test_parser (module M)

  let test_parsing _ =
    test ~model:"[x: [a: {y, z}]]"
      ~expected:"((x ((a (y z)))))";
    test ~model:"[x: [a: {}]]" ~expected:"((x ((a ()))))"
end

module Exp_dist_test = struct
  module M =
    Model_parser.Make
      (Exp_by_set.Make (Specs.Actions) (Distribution))

  let test = test_parser (module M)

  let test_parsing _ =
    test ~model:"[x: [a: [y: 1/2]]]"
      ~expected:"((x ((a ((y (1 2)))))))"
end

module Relational_test = struct
  module M =
    Model_parser.Make
      (Product.Make
         (Constant.Make
            (Ap_list_spec))
            (Exp_by_set.Make (Specs.Actions) (Powerset)))

  let test = test_parser (module M)

  let test_parsing _ =
    test ~model:"[]" ~expected:"()";
    test ~model:"[x: ({}, [a: {x, y}])]"
      ~expected:"((x (() ((a (x y))))))";
    test ~model:"[y: ({p}, [a: {x}])]"
      ~expected:"((y ((p) ((a (x))))))";
    test ~model:"[s0: ({p, q}, [a: {s1}])]"
      ~expected:"((s0 ((p q) ((a (s1))))))"
end

module Probabilistic_test = struct
  module M =
    Model_parser.Make
      (Product.Make
         (Constant.Make
            (Ap_list_spec))
            (Exp_by_set.Make (Specs.Actions) (Distribution)))

  let test = test_parser (module M)

  let test_parsing _ =
    test ~model:"[]" ~expected:"()";
    test ~model:"[x: ({p}, [a: [x: 2/3]])]"
      ~expected:"((x ((p) ((a ((x (2 3))))))))"
end

let suite =
  "Model_parsers"
  >::: [
         "Constant"
         >::: [ "parsing" >:: Constant_test.test_parsing ]
       ; "Powerset"
         >::: [ "parsing" >:: Powerset_test.test_parsing ]
       ; "Distribution"
         >::: [
                "parsing" >:: Distribution_test.test_parsing
              ]
       ; "Exp_by_set(Actions)(Powerset)"
         >::: [ "parsing" >:: Exp_pow_test.test_parsing ]
       ; "Exp_by_set(Actions)(Distribution)"
         >::: [ "parsing" >:: Exp_dist_test.test_parsing ]
       ; "Relational"
         >::: [ "parsing" >:: Relational_test.test_parsing ]
       ; "Probabilistic"
         >::: [
                "parsing"
                >:: Probabilistic_test.test_parsing
              ]
       ]

let () = run_test_tt_main suite
