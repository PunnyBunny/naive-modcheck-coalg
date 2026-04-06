module Model = struct
  let parse_relational_model =
    Relational_model_angstrom.parse_relational_model

  let parse_probabilistic_model =
    Probabilistic_model_angstrom.parse_probabilistic_model

  module Ast = Model_ast
end

module Formula = struct
  let parse_relational_formula =
    Relational_formula_angstrom.parse_relational_formula

  let parse_probabilistic_formula =
    Probabilistic_formula_angstrom
    .parse_probabilistic_formula

  module Ast = Formula_ast
end
