(* TODO: fix name *)
module Model = struct
  open Model_parser_wrapper
  open Model_ast

  let parse_relational_model = parse_relational_model
  let parse_graded_model = parse_graded_model
  let parse_probabilistic_model = parse_probabilistic_model
  let parse_monotone_model = parse_monotone_model

  module Relational_ast = Relational_ast
  module Graded_ast = Graded_ast
  module Probabilistic_ast = Probabilistic_ast
  module Monotone_ast = Monotone_ast
end
