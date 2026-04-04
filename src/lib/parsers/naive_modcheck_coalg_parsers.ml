module Model = struct
  open Model_parser_wrapper

  let parse_relational_model = parse_relational_model
  let parse_graded_model = parse_graded_model
  let parse_probabilistic_model = parse_probabilistic_model
  let parse_monotone_model = parse_monotone_model

  module Ast = Model_ast
end

module Formula = struct
  open Formula_parser_wrapper

  let parse_relational_formula = parse_relational_formula

  let parse_probabilistic_formula =
    parse_probabilistic_formula

  module Ast = Formula_ast
end
