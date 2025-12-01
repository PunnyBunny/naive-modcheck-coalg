%{
  open Core
  open Model_ast
%}

%token COLON
%token COMMA
%token SLASH
%token LBRACK
%token RBRACK
%token LNBRACE
%token RNBRACE
%token LCBRACE
%token RCBRACE
%token <string> IDENT
%token <int> INT
%token EOF

%start <Model_ast.Relational_ast.t> relational_model
%start <Model_ast.Graded_ast.t> graded_model
%start <Model_ast.Probabilistic_ast.t> probabilistic_model
%start <Model_ast.Monotone_ast.t> monotone_model

%%

// function: [x:a, y:b, ...]
// list: {a, b, ...}
// tuple: (a, b)

// the outermost function can be with or without brackets and commas
model(X): xi = func_with_or_without_bracket_and_comma(state, tuple(list(ap), func(action, X))) EOF { xi }

relational_model: xi = model(list(state)) EOF { xi };
graded_model: xi = model(func(state, INT)) EOF { xi };
probabilistic_model: xi = model(func(state, frac)) EOF { xi };
monotone_model: xi = model(list(list(state))) EOF { xi };

frac: n = INT SLASH d = INT { (n, d) }

tuple(A, B): LNBRACE a = A COMMA b = B RNBRACE { (a, b) }

list(A): LCBRACE l = list_inner(A) RCBRACE { l }

list_inner(A): 
  | { [] }
  | a = A { [a] }
  | a = A COMMA b = list_inner(A) { a :: b }

func_with_or_without_bracket_and_comma(A, B): 
  | f = func(A, B) { f } 
  | f = func_without_comma(A, B) { f }

func(A, B): LBRACK f = func_with_comma(A, B) RBRACK { f }

func_with_comma(A, B): // [x:a, y:b, ...]
  | { Hashtbl.Poly.create () }
  | a = A COLON b = B { 
      let tbl = Hashtbl.Poly.create () in
      Hashtbl.add_exn tbl ~key:a ~data:b; 
      tbl 
    }
  | a = A COLON b = B COMMA c = func_with_comma(A, B) { 
      Hashtbl.add_exn c ~key:a ~data:b; 
      c 
    }

func_without_comma(A, B): // x:a y:b ...
  | a = A COLON b = B { 
      let tbl = Hashtbl.Poly.create () in
      Hashtbl.add_exn tbl ~key:a ~data:b; 
      tbl 
    }
  | a = A COLON b = B c = func_without_comma(A, B) { 
      Hashtbl.add_exn c ~key:a ~data:b; 
      c 
    }

action: 
  | a = IDENT { Action.of_string a }
  | LCBRACE RCBRACE { Action.of_string "" } // allow empty action by {} in monomodal logic

state: s = IDENT { State.of_string s }

ap: ap = IDENT { Ap.of_string ap }
