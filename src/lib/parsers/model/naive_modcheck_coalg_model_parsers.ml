module Model_parser = Model_parser
module Relational_parser = Relational_parser
module Probabilistic_parser = Probabilistic_parser

let parse_relational_model = Relational_parser.parse_model

let parse_probabilistic_model =
  Probabilistic_parser.parse_model
