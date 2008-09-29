/* The PURE parser.  -*- C++ -*- */

// Tell bison that we want a C++ parser.
/* NOTE: We require at least bison 2.1a here, since the C++ parser skeleton
   changed several times, and the newer versions are not compatible with bison
   2.1 and earlier. :( */
%skeleton "lalr1.cc"
%require "2.1a"
%defines

%{
#include <iostream>
#include <string>
#include "expr.hh"
#include "printer.hh"
#include "util.hh"

#define action(task,cleanup) \
  try { if (interp.nerrs == 0) {task;} else {cleanup;} } \
  catch (err &e) { error(yyloc, e.what()); } \
  interp.nerrs = 0;

#define umin_action(x,y,z) \
  try { x = interp.uminop(y, z); } \
  catch (err &e) \
  { error(yylloc, e.what()); x = new expr(interp.symtab.void_sym().f); }

using namespace std;

class interpreter;
%}

// The parsing context.
%parse-param { interpreter& interp }
%lex-param   { interpreter& interp }

// Location tracking.
%locations
%initial-action
{
  static string stdin_source = "<stdin>";
  // Initialize the initial location.
  if (interp.source.empty())
    @$.begin.filename = @$.end.filename = &stdin_source;
  else
    @$.begin.filename = @$.end.filename = &interp.source;
};

// Enable parser tracing and verbose error messages.
%debug
%error-verbose

// Symbols.

%{
struct sym_info {
  bool priv;
  prec_t prec;
  fix_t fix;
  sym_info(bool v, prec_t p, fix_t f) : priv(v), prec(p), fix(f) { }
};
struct rule_info {
  exprl l;
  env e;
};
struct pat_rule_info {
  exprl l;
  rulel rl;
};
typedef pair<expr,expr> comp_clause;
typedef list<comp_clause> comp_clause_list;
%}

%union
{
  char    cval;
  int     ival;
  double  dval;
  mpz_t  *zval;
  string *sval;
  char   *csval;
  expr   *xval;
  exprl  *xlval;
  exprll *xllval;
  rule   *rval;
  rulel  *rlval;
  rule_info *rinfo;
  pat_rule_info *prinfo;
  list<string> *slval;
  comp_clause_list *clauselval;
  comp_clause *clauseval;
  fix_t   fix;
  sym_info *info;
};

%{
#include "interpreter.hh"
%}

%token		PRIVATE	"private"
%token		NULLARY	"nullary"
%token <fix>	FIX	"fixity"

%token		DEF	"def"
%token		CONST	"const"
%token		LET	"let"
%token		CASE	"case"
%token		OF	"of"
%token		END	"end"
%token		IF	"if"
%token		THEN	"then"
%token		ELSE	"else"
%token		OTHERWISE "otherwise"
%token		WHEN	"when"
%token		WITH	"with"
%token		USING	"using"
%token		EXTERN	"extern"

%token <xval>	NA0	"infix 0 operator"
%token <xval>	LT0	"infixl 0 operator"
%token <xval>	RT0	"infixr 0 operator"
%token <xval>	PR0	"prefix 0 operator"
%token <xval>	PO0	"postfix 0 operator"
%token <xval>	NA1	"infix 1 operator"
%token <xval>	LT1	"infixl 1 operator"
%token <xval>	RT1	"infixr 1 operator"
%token <xval>	PR1	"prefix 1 operator"
%token <xval>	PO1	"postfix 1 operator"
%token <xval>	NA2	"infix 2 operator"
%token <xval>	LT2	"infixl 2 operator"
%token <xval>	RT2	"infixr 2 operator"
%token <xval>	PR2	"prefix 2 operator"
%token <xval>	PO2	"postfix 2 operator"
%token <xval>	NA3	"infix 3 operator"
%token <xval>	LT3	"infixl 3 operator"
%token <xval>	RT3	"infixr 3 operator"
%token <xval>	PR3	"prefix 3 operator"
%token <xval>	PO3	"postfix 3 operator"
%token <xval>	NA4	"infix 4 operator"
%token <xval>	LT4	"infixl 4 operator"
%token <xval>	RT4	"infixr 4 operator"
%token <xval>	PR4	"prefix 4 operator"
%token <xval>	PO4	"postfix 4 operator"
%token <xval>	NA5	"infix 5 operator"
%token <xval>	LT5	"infixl 5 operator"
%token <xval>	RT5	"infixr 5 operator"
%token <xval>	PR5	"prefix 5 operator"
%token <xval>	PO5	"postfix 5 operator"
%token <xval>	NA6	"infix 6 operator"
%token <xval>	LT6	"infixl 6 operator"
%token <xval>	RT6	"infixr 6 operator"
%token <xval>	PR6	"prefix 6 operator"
%token <xval>	PO6	"postfix 6 operator"
%token <xval>	NA7	"infix 7 operator"
%token <xval>	LT7	"infixl 7 operator"
%token <xval>	RT7	"infixr 7 operator"
%token <xval>	PR7	"prefix 7 operator"
%token <xval>	PO7	"postfix 7 operator"
%token <xval>	NA8	"infix 8 operator"
%token <xval>	LT8	"infixl 8 operator"
%token <xval>	RT8	"infixr 8 operator"
%token <xval>	PR8	"prefix 8 operator"
%token <xval>	PO8	"postfix 8 operator"
%token <xval>	NA9	"infix 9 operator"
%token <xval>	LT9	"infixl 9 operator"
%token <xval>	RT9	"infixr 9 operator"
%token <xval>	PR9	"prefix 9 operator"
%token <xval>	PO9	"postfix 9 operator"

%right		MAPSTO
%left		WHEN WITH

%nonassoc	NA0
%left		LT0
%right		RT0
%left		PR0
%right		PO0
%nonassoc	NA1
%left		LT1
%right		RT1
%left		PR1
%right		PO1
%nonassoc	NA2
%left		LT2
%right		RT2
%left		PR2
%right		PO2
%nonassoc	NA3
%left		LT3
%right		RT3
%left		PR3
%right		PO3
%nonassoc	NA4
%left		LT4
%right		RT4
%left		PR4
%right		PO4
%nonassoc	NA5
%left		LT5
%right		RT5
%left		PR5
%right		PO5
%nonassoc	NA6
%left		LT6
%right		RT6
%left		PR6
%right		PO6
%nonassoc	NA7
%left		LT7
%right		RT7
%left		PR7
%right		PO7
%nonassoc	NA8
%left		LT8
%right		RT8
%left		PR8
%right		PO8
%nonassoc	NA9
%left		LT9
%right		RT9
%left		PR9
%right		PO9

%token		EOFTOK 0 "end of file"
%token		ERRTOK  "invalid character"
%token		BADTOK  "bad token"
%token		MAPSTO  "->"
%token <sval>	ID	"identifier"
%token <csval>	STR	"string"
%token <ival>	INT	"integer"
%token <zval>	BIGINT	"bigint"
%token <zval>	CBIGINT	"converted bigint"
%token <dval>	DBL	"floating point number"
%token <ival>	TAG	"type tag"
%type  <sval>	name optalias ctype
%type  <slval>	ids names ctypes opt_ctypes
%type  <info>	fixity
%type  <xval>	expr cond simple app prim op qual
%type  <xlval>	args lhs row
%type  <xllval>	rows row_list
%type  <clauselval>  comp_clauses comp_clause_list
%type  <clauseval>  comp_clause
%type  <rinfo>	rules rulel
%type  <prinfo>	pat_rules pat_rulel
%type  <rval>	simple_rule
%type  <rlval>	rule simple_rules simple_rulel

%destructor { delete $$; } ID fixity expr cond simple app prim op
  comp_clauses comp_clause_list rows row_list row args lhs qual
  rules rulel rule pat_rules pat_rulel simple_rules simple_rulel simple_rule
  ids names name optalias opt_ctypes ctypes ctype
%destructor { mpz_clear(*$$); free($$); } BIGINT CBIGINT
%destructor { free($$); } STR
%printer { debug_stream() << *$$; } ID name optalias ctype expr cond simple app
  prim op args lhs qual rule simple_rules simple_rulel simple_rule
%printer { debug_stream() << $$->e; } rules rulel
%printer { debug_stream() << $$->rl; } pat_rules pat_rulel
%printer { debug_stream() << $$; }  INT DBL STR
%printer { char *s = mpz_get_str(NULL, 10, *$$);
           debug_stream() << s; free(s); }  BIGINT CBIGINT

%%

// Pure grammar.

%start source;

/* An item in the source can either be an expression, a variable binding
   (let), a rewriting rule or an extern, fixity or using declaration. These
   must all be terminated with a semicolon. Current error recovery is
   panic-mode, which doesn't work too well. :( */

source
: /* empty */
| source ';'
| source item ';'
| error ';'		{ interp.nerrs = yyerrstatus_ = 0; }
;

item
: expr
{ action(interp.exec($1), delete $1); }
| LET simple_rule
{ action(interp.define($2), delete $2); }
| CONST simple_rule
{ action(interp.define_const($2), delete $2); }
| DEF simple_rule
{ action(interp.add_macro_rule($2), delete $2); }
| rule
{ rulel *rl = 0;
  action(interp.add_rules(interp.globenv,
  (rl = interp.default_lhs(interp.last, $1)), true), if (rl) delete rl); }
| fixity
/* Lexical tie-in: We need to tell the lexer that we're defining new operator
   symbols (interp.declare_op = true) instead of searching for existing ones
   in the symbol table. */
{ if ($1->priv && $1->prec > 10 ||
      !$1->priv && $1->fix != nullary && $1->prec > 9) {
    error(yylloc, "invalid fixity declaration"); YYERROR;
  } else if ($1->fix == nullary || $1->prec < 10)
    interp.declare_op = true; }
  ids
{ interp.declare_op = false;
  action(interp.declare($1->priv, $1->prec, $1->fix, $3), delete $3);
  delete $1; }
| USING names
{ action(interp.run(*$2), {}); delete $2; }
| EXTERN prototypes
;

fixity
: FIX INT		{ $$ = new sym_info(false,$2,$1); }
| NULLARY		{ $$ = new sym_info(false,10,nullary); }
| PRIVATE FIX INT	{ $$ = new sym_info(true,$3,$2); }
| PRIVATE NULLARY	{ $$ = new sym_info(true,10,nullary); }
| PRIVATE		{ $$ = new sym_info(true,10,infix); }
;

ids
: ID
{ $$ = new list<string>; $$->push_back(*$1); delete $1; }
| ids ID
{ $$ = $1; $$->push_back(*$2); delete $2; }
;

names
: name
{ $$ = new list<string>; $$->push_back(*$1); delete $1; }
| names ',' name
{ $$ = $1; $$->push_back(*$3); delete $3; }
;

name
: ID			{ $$ = $1; *$$ += ".pure"; }
| STR			{ char *s = fromutf8($1); free($1);
			  $$ = new string(s); free(s); }
;

prototypes
: prototype
| prototypes ',' prototype
;

prototype
: ctype ID '(' opt_ctypes ')' optalias
{ action(interp.declare_extern(*$2, *$1, *$4, false, 0, *$6), {});
  delete $1; delete $2; delete $4; delete $6; }
;

opt_ctypes
: /* empty */
{ $$ = new list<string>; }
| ctypes
;

ctypes
: ctype optname
{ $$ = new list<string>; $$->push_back(*$1); delete $1; }
| ctypes ',' ctype optname
{ $$ = $1; $$->push_back(*$3); delete $3; }
;

ctype
: ID
| ctype '*'		{ $$ = $1; *$$ += "*"; }
;

optname
: /* empty */
| ID			{ delete $1; }
;

optalias
: /* empty */		{ $$ = new string; }
| '=' ID		{ $$ = $2; }
;

/* Expression types are either simple expression (see below) or one of the
   special lambda, case, when/with and if-then-else constructs. Lambda binds
   most weakly, followed by when/with and case, followed by if-then-else and
   simple expressions. */

expr
: cond
| '\\' args MAPSTO expr
{ try { $$ = interp.mklambda_expr($2, $4); }
  catch (err &e) { interp.error(yyloc, e.what()); $$ = new expr; } }
| CASE cond OF pat_rules END
{ $$ = interp.mkcase_expr($2, new rulel($4->rl)); delete $4; }
| expr WHEN simple_rules END
{ try { $$ = interp.mkwhen_expr($1, $3); }
  catch (err &e) { interp.error(yyloc, e.what()); $$ = new expr; } }
| expr WITH rules END
{ $$ = interp.mkwith_expr($1, new env($3->e)); delete $3; }
;

cond
: simple
| IF simple THEN cond ELSE cond
{ $$ = interp.mkcond_expr($2, $4, $6); }
;

args
: prim
{ $$ = new exprl; $$->push_back(*$1); delete $1; }
| args prim
{ $$ = $1; $$->push_back(*$2); delete $2; }
;

/* Simple expressions (infix, prefix and postfix). This comprises the standard
   precedence levels 0..9. On each level, we have non-, left-, right-
   associative and unary prefix and postfix operators, in that order, from
   weakest to strongest. */

simple
: simple NA0 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple LT0 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple RT0 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| PR0 simple		{ $$ = interp.mkexpr($1, $2); }
| simple PO0		{ $$ = interp.mkexpr($2, $1); }
| simple NA1 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple LT1 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple RT1 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| PR1 simple		{ $$ = interp.mkexpr($1, $2); }
| simple PO1		{ $$ = interp.mkexpr($2, $1); }
| simple NA2 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple LT2 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple RT2 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| PR2 simple		{ $$ = interp.mkexpr($1, $2); }
| simple PO2		{ $$ = interp.mkexpr($2, $1); }
| simple NA3 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple LT3 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple RT3 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| PR3 simple		{ $$ = interp.mkexpr($1, $2); }
| simple PO3		{ $$ = interp.mkexpr($2, $1); }
| simple NA4 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple LT4 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple RT4 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| PR4 simple		{ $$ = interp.mkexpr($1, $2); }
| simple PO4		{ $$ = interp.mkexpr($2, $1); }
| simple NA5 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple LT5 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple RT5 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| PR5 simple		{ $$ = interp.mkexpr($1, $2); }
| simple PO5		{ $$ = interp.mkexpr($2, $1); }
| simple NA6 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple LT6 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple RT6 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| PR6 simple		{ $$ = interp.mkexpr($1, $2); }
| simple PO6		{ $$ = interp.mkexpr($2, $1); }
| simple NA7 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple LT7 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple RT7 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| PR7 simple		{ $$ = interp.mkexpr($1, $2); }
| simple PO7		{ $$ = interp.mkexpr($2, $1); }
| simple NA8 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple LT8 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple RT8 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| PR8 simple		{ $$ = interp.mkexpr($1, $2); }
| simple PO8		{ $$ = interp.mkexpr($2, $1); }
| simple NA9 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple LT9 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| simple RT9 simple	{ $$ = interp.mkexpr($2, $1, $3); }
| PR9 simple		{ $$ = interp.mkexpr($1, $2); }
| simple PO9		{ $$ = interp.mkexpr($2, $1); }

/* Special case rules for unary minus. This requires that an infix minus
   operator has been defined already, and will automagically make unary minus
   a prefix operator on the same precedence level as the binary operator. */
| NA0 simple %prec PR0	{ umin_action($$, $1, $2); }
| NA1 simple %prec PR1	{ umin_action($$, $1, $2); }
| NA2 simple %prec PR2	{ umin_action($$, $1, $2); }
| NA3 simple %prec PR3	{ umin_action($$, $1, $2); }
| NA4 simple %prec PR4	{ umin_action($$, $1, $2); }
| NA5 simple %prec PR5	{ umin_action($$, $1, $2); }
| NA6 simple %prec PR6	{ umin_action($$, $1, $2); }
| NA7 simple %prec PR7	{ umin_action($$, $1, $2); }
| NA8 simple %prec PR8	{ umin_action($$, $1, $2); }
| NA9 simple %prec PR9	{ umin_action($$, $1, $2); }
| LT0 simple %prec PR0	{ umin_action($$, $1, $2); }
| LT1 simple %prec PR1	{ umin_action($$, $1, $2); }
| LT2 simple %prec PR2	{ umin_action($$, $1, $2); }
| LT3 simple %prec PR3	{ umin_action($$, $1, $2); }
| LT4 simple %prec PR4	{ umin_action($$, $1, $2); }
| LT5 simple %prec PR5	{ umin_action($$, $1, $2); }
| LT6 simple %prec PR6	{ umin_action($$, $1, $2); }
| LT7 simple %prec PR7	{ umin_action($$, $1, $2); }
| LT8 simple %prec PR8	{ umin_action($$, $1, $2); }
| LT9 simple %prec PR9	{ umin_action($$, $1, $2); }
| RT0 simple %prec PR0	{ umin_action($$, $1, $2); }
| RT1 simple %prec PR1	{ umin_action($$, $1, $2); }
| RT2 simple %prec PR2	{ umin_action($$, $1, $2); }
| RT3 simple %prec PR3	{ umin_action($$, $1, $2); }
| RT4 simple %prec PR4	{ umin_action($$, $1, $2); }
| RT5 simple %prec PR5	{ umin_action($$, $1, $2); }
| RT6 simple %prec PR6	{ umin_action($$, $1, $2); }
| RT7 simple %prec PR7	{ umin_action($$, $1, $2); }
| RT8 simple %prec PR8	{ umin_action($$, $1, $2); }
| RT9 simple %prec PR9	{ umin_action($$, $1, $2); }

| app
;

/* Applications and primary expressions. */

app
: prim
| app prim		{ $$ = interp.mkexpr($1, $2); }
;

prim
: ID			{ $$ = interp.mksym_expr($1); }
| ID TAG		{ try { $$ = interp.mksym_expr($1, $2); }
			  catch (err &e) {
			    interp.error(yyloc, e.what());
			    $$ = interp.mksym_expr($1);
			  } }
| ID '@' prim		{ try { $$ = interp.mkas_expr($1, $3); }
			  catch (err &e) {
			    interp.error(yyloc, e.what());
			    $$ = $3;
			  } }
| INT			{ $$ = new expr(EXPR::INT, $1); }
| CBIGINT		{ $$ = new expr(EXPR::BIGINT, *$1, true); free($1); }
| BIGINT		{ $$ = new expr(EXPR::BIGINT, *$1); free($1); }
| DBL			{ $$ = new expr(EXPR::DBL, $1); }
| STR			{ $$ = new expr(EXPR::STR, $1); }
| '{' rows '}'		{ $$ = new expr(EXPR::MATRIX, $2); }
| '{' expr '|' comp_clauses '}'
			{ $$ = interp.mkmatcomp_expr($2, $4); }
| '[' expr ']'		{ $$ = interp.mklist_expr($2); }
| '[' expr ';' comp_clauses ']'
			{ interp.warning(yyloc,
			    "warning: deprecated comprehension syntax");
			  $$ = interp.mklistcomp_expr($2, $4); }
| '[' expr '|' comp_clauses ']'
			{ $$ = interp.mklistcomp_expr($2, $4); }
| '(' expr ')'		{ $$ = $2;
			  if ($$->is_pair()) $$->flags() |= EXPR::PAREN; }
| '(' op ')'		{ $$ = $2; }
;

comp_clauses
: comp_clause_list
| comp_clause_list ';'	{ $$ = $1; }
;

comp_clause_list
: comp_clause
{ $$ = new comp_clause_list; $$->push_back(*$1); delete $1; }
| comp_clause_list ';' comp_clause
{ $$ = $1; $$->push_back(*$3); delete $3; }
;

comp_clause
: expr
{ $$ = new comp_clause(*$1, expr()); delete $1; }
| expr '=' expr
{ $$ = new comp_clause(*$1, *$3); delete $1; delete $3; }
;

rows
: row_list
| row_list ';'	{ $$ = $1; }
| /* empty */	{ $$ = new exprll; }
;

row_list
: row
{ $$ = new exprll; $$->push_back(*$1); delete $1; }
| row_list ';' row
{ $$ = $1; $$->push_back(*$3); delete $3; }
;

row
: expr		{ $$ = interp.mkrow_exprl($1); }
;

op
: NA0 | LT0 | RT0 | PR0 | PO0
| NA1 | LT1 | RT1 | PR1 | PO1
| NA2 | LT2 | RT2 | PR2 | PO2
| NA3 | LT3 | RT3 | PR3 | PO3
| NA4 | LT4 | RT4 | PR4 | PO4
| NA5 | LT5 | RT5 | PR5 | PO5
| NA6 | LT6 | RT6 | PR6 | PO6
| NA7 | LT7 | RT7 | PR7 | PO7
| NA8 | LT8 | RT8 | PR8 | PO8
| NA9 | LT9 | RT9 | PR9 | PO9
;

/* Rewriting rule syntax. These generally take the form l = r [if g]; ... For
   convenience, we also allow a semicolon at the end of a rule list. Moreover,
   multiple left-hand sides are permitted (denoting a collection of rules for
   the same right-hand side), and the left-hand side may also be omitted, in
   which case the left-hand sides of the previous rule are repeated. */

rule
: lhs '=' expr qual
{ $$ = new rulel;
  for (exprl::iterator l = $1->begin(), end = $1->end(); l != end; l++)
    $$->push_back(rule(*l, *$3, *$4));
  delete $1; delete $3; delete $4; }
| '=' expr qual
{ $$ = new rulel(1, rule(expr(), *$2, *$3)); delete $2; delete $3; }
;

lhs
: expr
{ $$ = new exprl; $$->push_back(*$1); delete $1; }
| lhs '|' expr
{ $$ = $1; $$->push_back(*$3); delete $3; }
;

qual
: /* empty */		{ $$ = new expr(); }
| OTHERWISE		{ $$ = new expr(); }
| IF simple		{ $$ = $2; }
;

rules
: rulel
| rulel ';'		{ $$ = $1; }
;

rulel
: rule
{ $$ = new rule_info;
  rulel *rl = 0;
  try {
    rl = interp.default_lhs($$->l, $1);
    interp.add_rules($$->e, rl);
  }
  catch (err &e) { if (rl) delete rl; interp.error(yyloc, e.what()); } }
| rulel ';' rule
{ $$ = $1;
  rulel *rl = 0;
  try {
    rl = interp.default_lhs($$->l, $3);
    interp.add_rules($$->e, rl);
  }
  catch (err &e) { if (rl) delete rl; interp.error(yyloc, e.what()); } }
;

/* Same for pattern rules (pattern binding in 'case' clauses). */

pat_rules
: pat_rulel
| pat_rulel ';'		{ $$ = $1; }
;

pat_rulel
: rule
{ $$ = new pat_rule_info;
  rulel *rl = 0;
  try {
    rl = interp.default_lhs($$->l, $1);
    interp.add_rules($$->rl, rl, true);
  }
  catch (err &e) { if (rl) delete rl; interp.error(yyloc, e.what()); } }
| pat_rulel ';' rule
{ $$ = $1;
  rulel *rl = 0;
  try {
    rl = interp.default_lhs($$->l, $3);
    interp.add_rules($$->rl, rl, true);
  }
  catch (err &e) { if (rl) delete rl; interp.error(yyloc, e.what()); } }
;

/* Same for simple rules (pattern binding in 'when' clauses or 'let', 'const',
   'def', no guards in these cases). */

simple_rule
: expr '=' expr
{ $$ = new rule(*$1, *$3); delete $1; delete $3; }
;

simple_rules
: simple_rulel
| simple_rulel ';'		{ $$ = $1; }
;

simple_rulel
: simple_rule
{ $$ = new rulel; try { interp.add_simple_rule(*$$, $1); }
  catch (err &e) { interp.error(yyloc, e.what()); } }
| simple_rulel ';' simple_rule
{ $$ = $1; try { interp.add_simple_rule(*$$, $3); }
  catch (err &e) { interp.error(yyloc, e.what()); } }
;

%%

void
yy::parser::error (const yy::parser::location_type& l,
		   const string& m)
{
  interp.error(l, m);
}
