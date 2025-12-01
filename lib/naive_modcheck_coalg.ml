include struct
  open Model_parser_wrapper

  let parse_relational_model = parse_relational_model
  let parse_graded_model = parse_graded_model
  let parse_probabilistic_model = parse_probabilistic_model
  let parse_monotone_model = parse_monotone_model
end

include struct
  module Relational_ast = Model_ast.Relational_ast
  module Graded_ast = Model_ast.Graded_ast
  module Probabilistic_ast = Model_ast.Probabilistic_ast
  module Monotone_ast = Model_ast.Monotone_ast
  module Action = Model_ast.Action
  module Ap = Model_ast.Ap
  module State = Model_ast.State
end
