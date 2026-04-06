open Model_angstrom

module P = Make (Model_ast.Relational_ast)

let parse_relational_model =
  P.make_model_parser ~transition:(list_of state)
