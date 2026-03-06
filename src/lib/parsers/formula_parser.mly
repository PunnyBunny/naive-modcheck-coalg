%{
  open Naive_modcheck_coalg_common

  (* Track bound fixpoint variables during parsing so that IDENT tokens
     that refer to a binder are emitted as Var, not Ap. *)
  let _bound : string list ref = ref []
  let _push x = _bound := x :: !_bound
  let _pop () = match !_bound with _ :: rest -> _bound := rest | [] -> ()
  let _is_bound x = Stdlib.List.mem x !_bound
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
%token SLASH
%token <int> INT
%token DOT
%token LPAREN
%token RPAREN
%token <string> IDENT
%token EOF

%left OR
%left AND
%nonassoc NOT

%start <Formula_ast.Relational_ast.t> relational_formula
%start <Formula_ast.Probabilistic_ast.t> probabilistic_formula

%%

(* Binder prefixes: reduced before the body is parsed, so
   the variable is visible during body parsing. *)

mu_binder:
  | MU x = IDENT DOT { _push x; x }

nu_binder:
  | NU x = IDENT DOT { _push x; x }

relational_formula:
  | f = rel_formula EOF { _bound := []; f }

rel_formula:
  | TRUE { Formula_ast.Relational_ast.True }
  | FALSE { Formula_ast.Relational_ast.False }
  | NOT f = rel_formula { Formula_ast.Relational_ast.Not f }
  | LPAREN f = rel_formula RPAREN { f }
  | f1 = rel_formula AND f2 = rel_formula { Formula_ast.Relational_ast.And (f1, f2) }
  | f1 = rel_formula OR f2 = rel_formula { Formula_ast.Relational_ast.Or (f1, f2) }
  | LANGLE a = action RANGLE f = rel_formula { Formula_ast.Relational_ast.Diamond (a, (), f) }
  | LBRACK a = action RBRACK f = rel_formula { Formula_ast.Relational_ast.Box (a, (), f) }
  | x = mu_binder f = rel_formula { _pop (); Formula_ast.Relational_ast.Mu (Var.of_string x, f) }
  | x = nu_binder f = rel_formula { _pop (); Formula_ast.Relational_ast.Nu (Var.of_string x, f) }
  | p = IDENT {
      if _is_bound p then Formula_ast.Relational_ast.Var (Var.of_string p)
      else Formula_ast.Relational_ast.Ap (Ap.of_string p)
    }

probabilistic_formula:
  | f = prob_formula EOF { _bound := []; f }

prob_formula:
  | TRUE { Formula_ast.Probabilistic_ast.True }
  | FALSE { Formula_ast.Probabilistic_ast.False }
  | NOT f = prob_formula { Formula_ast.Probabilistic_ast.Not f }
  | LPAREN f = prob_formula RPAREN { f }
  | f1 = prob_formula AND f2 = prob_formula { Formula_ast.Probabilistic_ast.And (f1, f2) }
  | f1 = prob_formula OR f2 = prob_formula { Formula_ast.Probabilistic_ast.Or (f1, f2) }
  | LANGLE n = INT SLASH d = INT a = action RANGLE f = prob_formula { Formula_ast.Probabilistic_ast.Diamond (a, (Frac.make n d), f) }
  | LBRACK n = INT SLASH d = INT a = action RBRACK f = prob_formula { Formula_ast.Probabilistic_ast.Box (a, (Frac.make n d), f) }
  | x = mu_binder f = prob_formula { _pop (); Formula_ast.Probabilistic_ast.Mu (Var.of_string x, f) }
  | x = nu_binder f = prob_formula { _pop (); Formula_ast.Probabilistic_ast.Nu (Var.of_string x, f) }
  | p = IDENT {
      if _is_bound p then Formula_ast.Probabilistic_ast.Var (Var.of_string p)
      else Formula_ast.Probabilistic_ast.Ap (Ap.of_string p)
    }

action:
  | { Action.of_string "" } (* empty action for monomodal logic *)
  | a = IDENT { Action.of_string a }
