open Model_angstrom

module P = Make (Model_ast.Probabilistic_ast)

let parse_probabilistic_model =
  P.make_model_parser ~transition:(func_of state frac)
