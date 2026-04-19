open! Core
open OUnit2
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_formula
open Naive_modcheck_coalg_parsers_formula.Formula_parsers
open Naive_modcheck_coalg_logics

let test_parser (module F : Formula.S) ~expected ~formula =
  assert_equal ~cmp:Sexp.equal ~printer:Sexp.to_string_hum
    (Sexp.of_string expected)
    (F.parse_formula formula |> [%sexp_of: F.t])

module Constant_test = struct
  module F = Formula_parser.Make (Constant.Make (Specs.Ap))

  let test = test_parser (module F)

  let test_atoms _ =
    test ~formula:"p" ~expected:"(Modal (Normal p))";
    test ~formula:"!p" ~expected:"(Modal (Not p))";
    (* Constant.parser greedily matches any word, so "true"/"false" are treated
       as atomic propositions, not boolean constants. *)
    test ~formula:"true" ~expected:"(Modal (Normal true))";
    test ~formula:"false" ~expected:"(Modal (Normal false))"
  (* TODO: probably can't bind vars. But fixpoint with just constant outer level is not useful. *)

  let test_connectives _ =
    test ~formula:"!p && !q"
      ~expected:"(And (Modal (Not p)) (Modal (Not q)))";
    test ~formula:"p || q"
      ~expected:"(Or (Modal (Normal p)) (Modal (Normal q)))"

  let test_negation _ =
    test ~formula:"~p" ~expected:"(Modal (Not p))";
    test ~formula:"~!p" ~expected:"(Modal (Normal p))";
    test ~formula:"~(p && !q)"
      ~expected:"(Or (Modal (Not p)) (Modal (Normal q)))"
end

module Powerset_test = struct
  module F = Formula_parser.Make (Powerset)

  let test = test_parser (module F)

  let test_modalities _ =
    test ~formula:"<>p"
      ~expected:"(Modal (Diamond (Var p)))";
    test ~formula:"[]p" ~expected:"(Modal (Box (Var p)))"

  let test_fixpoint _ =
    test ~formula:"nu X. []X && p"
      ~expected:"(Nu X (Modal (Box (And (Var X) (Var p)))))"

  let test_nested _ =
    test ~formula:"[]<>p"
      ~expected:"(Modal (Box (Modal (Diamond (Var p)))))"

  let test_negation _ =
    test ~formula:"~<>p" ~expected:"(Modal (Box (Var p)))";
    test ~formula:"~[]p"
      ~expected:"(Modal (Diamond (Var p)))";
    test ~formula:"~~<>p"
      ~expected:"(Modal (Diamond (Var p)))";
    test ~formula:"~<>[]p"
      ~expected:"(Modal (Box (Modal (Diamond (Var p)))))";
    test ~formula:"~((<>p) && ([]q))"
      ~expected:
        "(Or (Modal (Box (Var p))) (Modal (Diamond (Var \
         q))))";
    test ~formula:"~(nu X. []X)"
      ~expected:"(Mu X (Modal (Diamond (Var X))))"
end

module Distribution_test = struct
  module F = Formula_parser.Make (Distribution)

  let test = test_parser (module F)

  let test_modalities _ =
    test ~formula:"[>=1/2]p"
      ~expected:"(Modal (GE (1 2) (Var p)))";
    test ~formula:"[>1/3]p"
      ~expected:"(Modal (GT (1 3) (Var p)))"

  let test_fixpoint _ =
    test ~formula:"mu X. [>=1/2]X || p"
      ~expected:
        "(Mu X (Modal (GE (1 2) (Or (Var X) (Var p)))))"

  let test_nested _ =
    test ~formula:"[>5/7][>=2/3]p"
      ~expected:
        "(Modal (GT (5 7) (Modal (GE (2 3) (Var p)))))"

  let test_edge_prob _ =
    test ~formula:"[>1]p"
      ~expected:"(Modal (GT (1 1) (Var p)))";
    test ~formula:"[>=0]p"
      ~expected:"(Modal (GE (0 1) (Var p)))"

  let test_negation _ =
    test ~formula:"~[>=1/3]p"
      ~expected:"(Modal (GT (2 3) (Var p)))";
    test ~formula:"~[>1/4]p"
      ~expected:"(Modal (GE (3 4) (Var p)))";
    test ~formula:"~~[>=1/3]p"
      ~expected:"(Modal (GE (1 3) (Var p)))";
    test ~formula:"~[>=1/2][>1/3]p"
      ~expected:
        "(Modal (GT (1 2) (Modal (GE (2 3) (Var p)))))";
    test ~formula:"~(([>=1/2]p) || ([>1/3]q))"
      ~expected:
        "(And (Modal (GT (1 2) (Var p))) (Modal (GE (2 3) \
         (Var q))))";
    test ~formula:"~(mu X. [>=1/2]X)"
      ~expected:"(Nu X (Modal (GT (1 2) (Var X))))"
end

module Exp_pow_test = struct
  module F =
    Formula_parser.Make
      (Exp_by_set.Make (Specs.Actions) (Powerset))

  let test = test_parser (module F)

  let test_modal _ =
    test ~formula:"[a]<>p"
      ~expected:"(Modal (a (Modal (Diamond (Var p)))))";
    test ~formula:"[a][]p"
      ~expected:"(Modal (a (Modal (Box (Var p)))))";
    test ~formula:"[aaa]true" ~expected:"(Modal (aaa True))";
    test ~formula:"[aaa]false"
      ~expected:"(Modal (aaa False))";
    test ~formula:"mu p . [b]<>p"
      ~expected:
        "(Mu p (Modal (b (Modal (Diamond (Var p))))))"

  let test_negation _ =
    test ~formula:"~[a]<>p"
      ~expected:"(Modal (a (Modal (Box (Var p)))))";
    test ~formula:"~[a][]p"
      ~expected:"(Modal (a (Modal (Diamond (Var p)))))"
end

module Exp_dist_test = struct
  module F =
    Formula_parser.Make
      (Exp_by_set.Make (Specs.Actions) (Distribution))

  let test = test_parser (module F)

  let test_modal _ =
    test ~formula:"[a][>=1/2]p"
      ~expected:"(Modal (a (Modal (GE (1 2) (Var p)))))";
    test ~formula:"[a][>3/4]p"
      ~expected:"(Modal (a (Modal (GT (3 4) (Var p)))))";
    test ~formula:"mu p . [b][>=5/7]p"
      ~expected:
        "(Mu p (Modal (b (Modal (GE (5 7) (Var p))))))"

  let test_negation _ =
    test ~formula:"~[a][>=1/2]p"
      ~expected:"(Modal (a (Modal (GT (1 2) (Var p)))))";
    test ~formula:"~[a][>1/4]p"
      ~expected:"(Modal (a (Modal (GE (3 4) (Var p)))))"
end

module Simple_product_test = struct
  module F =
    Formula_parser.Make
      (Product.Make (Constant.Make (Specs.Ap)) (Powerset))

  let test = test_parser (module F)

  let test_dispatch _ =
    test ~formula:"[@1]p"
      ~expected:"(Modal (A (Modal (Normal p))))";
    test ~formula:"[@1]!p"
      ~expected:"(Modal (A (Modal (Not p))))";
    test ~formula:"[@2]<>p"
      ~expected:"(Modal (B (Modal (Diamond (Var p)))))";
    test ~formula:"[@1]p && [@2]<>p"
      ~expected:
        "(And (Modal (A (Modal (Normal p)))) (Modal (B \
         (Modal (Diamond (Var p))))))"

  let test_negation _ =
    test ~formula:"~[@1]p"
      ~expected:"(Modal (A (Modal (Not p))))";
    test ~formula:"~[@2]<>p"
      ~expected:"(Modal (B (Modal (Box (Var p)))))"
end

module Relational_test = struct
  module F =
    Formula_parser.Make
      (Product.Make
         (Constant.Make
            (Specs.Ap))
            (Exp_by_set.Make (Specs.Actions) (Powerset)))

  let test = test_parser (module F)

  let test_atoms _ =
    test ~formula:"[@1]p"
      ~expected:"(Modal (A (Modal (Normal p))))";
    test ~formula:"[@1]!p"
      ~expected:"(Modal (A (Modal (Not p))))"

  let test_modal _ =
    test ~formula:"[@2][a]<>p"
      ~expected:
        "(Modal (B (Modal (a (Modal (Diamond (Var p)))))))"

  let test_connective _ =
    test ~formula:"[@1]p && [@2][a]<>p"
      ~expected:
        "(And (Modal (A (Modal (Normal p)))) (Modal (B \
         (Modal (a (Modal (Diamond (Var p))))))))"

  let test_fixpoint _ =
    test ~formula:"nu x1. ([@1]p & [@2][a]<> x1)"
      ~expected:
        "(Nu x1 (And (Modal (A (Modal (Normal p)))) (Modal \
         (B (Modal (a (Modal (Diamond (Var x1)))))))))"

  let test_negation _ =
    test ~formula:"~[@1]p"
      ~expected:"(Modal (A (Modal (Not p))))";
    test ~formula:"~[@2][a]<>p"
      ~expected:
        "(Modal (B (Modal (a (Modal (Box (Var p)))))))"
end

module Probabilistic_test = struct
  module F =
    Formula_parser.Make
      (Product.Make
         (Constant.Make
            (Specs.Ap))
            (Exp_by_set.Make (Specs.Actions) (Distribution)))

  let test = test_parser (module F)

  let test_atoms _ =
    test ~formula:"[@1]p"
      ~expected:"(Modal (A (Modal (Normal p))))"

  let test_modal _ =
    test ~formula:"[@2][a][>=1/2]p"
      ~expected:
        "(Modal (B (Modal (a (Modal (GE (1 2) (Var p)))))))"

  let test_fixpoint _ =
    test ~formula:"nu x. ([@1]p & [@2][a][>=1/2] x)"
      ~expected:
        "(Nu x (And (Modal (A (Modal (Normal p)))) (Modal \
         (B (Modal (a (Modal (GE (1 2) (Var x)))))))))"

  let test_negation _ =
    test ~formula:"~[@1]p"
      ~expected:"(Modal (A (Modal (Not p))))";
    test ~formula:"~[@2][a][>=1/2]p"
      ~expected:
        "(Modal (B (Modal (a (Modal (GT (1 2) (Var p)))))))"
end

let suite =
  "Formula_parsers"
  >::: [
         "Constant"
         >::: [
                "atoms" >:: Constant_test.test_atoms
              ; "connectives"
                >:: Constant_test.test_connectives
              ; "negation" >:: Constant_test.test_negation
              ]
       ; "Powerset"
         >::: [
                "modalities"
                >:: Powerset_test.test_modalities
              ; "fixpoint" >:: Powerset_test.test_fixpoint
              ; "nested" >:: Powerset_test.test_nested
              ; "negation" >:: Powerset_test.test_negation
              ]
       ; "Distribution"
         >::: [
                "modalities"
                >:: Distribution_test.test_modalities
              ; "fixpoint"
                >:: Distribution_test.test_fixpoint
              ; "nested" >:: Distribution_test.test_nested
              ; "edge cases"
                >:: Distribution_test.test_edge_prob
              ; "negation"
                >:: Distribution_test.test_negation
              ]
       ; "Exp_by_set(Actions)(Powerset)"
         >::: [
                "modal" >:: Exp_pow_test.test_modal
              ; "negation" >:: Exp_pow_test.test_negation
              ]
       ; "Exp_by_set(Actions)(Distribution)"
         >::: [
                "modal" >:: Exp_dist_test.test_modal
              ; "negation" >:: Exp_dist_test.test_negation
              ]
       ; "Product(Constant)(Powerset)"
         >::: [
                "dispatch"
                >:: Simple_product_test.test_dispatch
              ; "negation"
                >:: Simple_product_test.test_negation
              ]
       ; "Relational"
         >::: [
                "atoms" >:: Relational_test.test_atoms
              ; "modal" >:: Relational_test.test_modal
              ; "connective"
                >:: Relational_test.test_connective
              ; "fixpoint" >:: Relational_test.test_fixpoint
              ; "negation" >:: Relational_test.test_negation
              ]
       ; "Probabilistic"
         >::: [
                "atoms" >:: Probabilistic_test.test_atoms
              ; "modal" >:: Probabilistic_test.test_modal
              ; "fixpoint"
                >:: Probabilistic_test.test_fixpoint
              ; "negation"
                >:: Probabilistic_test.test_negation
              ]
       ]

let () = run_test_tt_main suite
