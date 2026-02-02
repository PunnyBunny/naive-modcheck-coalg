%{
  open Naive_modcheck_coalg_common
%}

%token TRUE
%token FALSE
%token AND
%token OR
%token NOT
%token MU
%token NU
%token LANGLE
%token RANGLE
%token LBRACK
%token RBRACK
%token DOT
%token LPAREN
%token RPAREN
%token <string> IDENT
%token EOF

%left OR
%left AND
%nonassoc NOT

%start <Formula_ast.Relational_ast.t> relational_formula

%%

relational_formula: 
  | f = formula EOF { f }

formula:
  | TRUE { Formula_ast.Relational_ast.True }
  | FALSE { Formula_ast.Relational_ast.False }
  | NOT f = formula { Formula_ast.Relational_ast.Not f }
  | LPAREN f = formula RPAREN { f }
  | f1 = formula AND f2 = formula { Formula_ast.Relational_ast.And (f1, f2) }
  | f1 = formula OR f2 = formula { Formula_ast.Relational_ast.Or (f1, f2) }
  | LANGLE a = action RANGLE f = formula { Formula_ast.Relational_ast.Diamond (a, (), f) }
  | LBRACK a = action RBRACK f = formula { Formula_ast.Relational_ast.Box (a, (), f) }
  | MU x = IDENT DOT f = formula { Formula_ast.Relational_ast.Mu (Var.of_string x, f) }
  | NU x = IDENT DOT f = formula { Formula_ast.Relational_ast.Nu (Var.of_string x, f) }
  | p = IDENT { Formula_ast.Relational_ast.Ap (Ap.of_string p) }

action:
  | { Action.of_string "" } (* empty action for monomodal logic *)
  | a = IDENT { Action.of_string a }
