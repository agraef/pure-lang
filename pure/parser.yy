/* The PURE parser.  -*- C++ -*- */

/* Copyright (c) 2008-2012 by Albert Graef <Dr.Graef@t-online.de>.

   This file is part of the Pure runtime.

   The Pure runtime is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at your
   option) any later version.

   Pure is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
   more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. */

// Tell bison that we want a C++ parser.
/* NOTE: We require at least bison 2.1a here, since the C++ parser skeleton
   changed several times, and the newer versions are not compatible with bison
   2.1 and earlier. :( */
%skeleton "lalr1.cc"
%require "2.2"
%defines

/* The operator section syntax causes a number of shift/reduce conflicts, we
   take care of these here. */
%expect 5

%{
#include <iostream>
#include <string>
#include "parserdefs.hh"
#include "printer.hh"
#include "util.hh"

// Get rid of silly warnings in bison-generated position.hh.
#pragma GCC diagnostic ignored "-Wparentheses"

#define restricted_action(task,cleanup) \
  try { if (interp.nerrs == 0) {task;} else {cleanup;} } \
  catch (err &e) { error(yyloc, e.what()); } \
  interp.nerrs = 0;

#define action(task,cleanup) \
  try { if (interp.restricted) throw err("operation not implemented"); \
        else if (interp.nerrs == 0) {task;} else {cleanup;} } \
  catch (err &e) { error(yyloc, e.what()); } \
  interp.nerrs = 0;

#define parser_action(task,cleanup) \
  try { task; }	catch (err &e) { cleanup; error(yyloc, e.what()); YYERROR; }

#define dummy_expr expr(interp.symtab.anon_sym)

#define priv_def (interp.active_namespaces.empty()?false:interp.active_namespaces.front().priv)

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
%define parse.error verbose

// Symbols.

%union
{
  char    cval;
  int32_t ival;
  double  dval;
  mpz_t  *zval;
  string *sval;
  char   *csval;
  expr   *xval;
  exprl  *xlval;
  exprll *xllval;
  OpStack *opstk;
  rule   *rval;
  rulel  *rlval;
  rhs_info *rhsval;
  rule_info *rinfo;
  pat_rule_info *prinfo;
  list<string> *slval;
  pair< bool, list<string> > *sloval;
  list<int32_t> *ilval;
  pair< string, list<int32_t> > *smval;
  list< pair< string, list<int32_t> > > *smlval;
  comp_clause_list *clauselval;
  comp_clause *clauseval;
  fix_t   fix;
  sym_info *info;
};

%{
// Work around undefined symbols in C++ parsers generated with Bison 3.
// NOTE: This setting should be determined automatically when the parser is
// regenerated.
#if HAVE_BISON3
// NOTE: This API keeps changing throughout Bison 3.x. type/type_get() is
// apparently being phased out in favour of kind(), so we might have to change
// this again in the future. For now (as of Bison 3.6.2), type_get() is still
// supported for backward compatibility, and this should hopefully work for
// all earlier 3.x releases of Bison (fingers crossed).
#define yychar yyla.type_get()
#define yylloc yyla.location
#define yyloc yylhs.location
#endif

#include "lexerdefs.hh"
#include "interpreter.hh"
static int extern_priv;
static void mangle_fname(string& name);
%}

%token		PRIVATE	"private"
%token		PUBLIC	"public"
%token		NONFIX	"nonfix"
%token		OUTFIX	"outfix"
%token <fix>	FIX	"fixity"

%token		DEF	"def"
%token		CONST	"const"
%token		LET	"let"
%token		TYPE	"type"
%token		INTERFACE "interface"
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
%token		NAMESPACE "namespace"
%token		EXTERN	"extern"

%token <xval>	LO	"left outfix operator"
%token <xval>	RO	"right outfix operator"
%token <xval>	NA	"infix operator"
%token <xval>	LT	"infixl operator"
%token <xval>	RT	"infixr operator"
%token <xval>	PR	"prefix operator"
%token <xval>	PO	"postfix operator"

%right		MAPSTO
%left		WHEN WITH ELSE

%token		EOFTOK 0 "end of file"
%token		ERRTOK  "invalid character"
%token		BADTOK  "bad token"
%token		MAPSTO	"->"
%token		ELLIPSIS "..."
%token		ESCAPE
%token		EOX	"end of input"
%token		CODE	"code section"
%token <sval>	ID	"identifier"
%token <ival>	XID	"symbol"
%token <csval>	STR	"string"
%token <ival>	INT	"integer"
%token <zval>	BIGINT	"bigint"
%token <zval>	CBIGINT	"converted bigint"
%token <dval>	DBL	"floating point number"
%token <ival>	TAG	"type tag"
%type  <sval>	name fname optalias ctype
%type  <slval>	ids fnames
%type  <sloval>	ctypes opt_ctypes
%type  <ilval>  xsyms
%type  <smval>	name_xsyms
%type  <smlval>	name_xsyms_list
%type  <ival>	op scope interface_rules
%type  <ival>	opt_brackets "namespace brackets"
%type  <info>	fixity
%type  <xval>	expr prim
%type  <opstk>  simple  "simple expression"
%type  <rhsval> rhs qual_rhs
%type  <xlval>	args lhs row
%type  <xllval>	rows row_list
%type  <clauselval>  comp_clauses comp_clause_list
%type  <clauseval>  comp_clause
%type  <rinfo>	rules rulel
%type  <prinfo>	pat_rules pat_rulel
%type  <rval>	simple_rule
%type  <rlval>	rule type_rule macro_rule simple_rules simple_rulel

%destructor { delete $$; } ID LO RO NA LT RT PR PO fixity expr simple prim
  comp_clauses comp_clause_list rows row_list row args lhs rhs qual_rhs
  rules rulel rule type_rule macro_rule pat_rules pat_rulel
  simple_rules simple_rulel simple_rule
  ids fnames fname name_xsyms_list name_xsyms xsyms name optalias opt_ctypes
  ctypes ctype
%destructor { mpz_clear(*$$); free($$); } BIGINT CBIGINT
%destructor { free($$); } STR
%printer { debug_stream() << *$$; } ID name fname optalias ctype expr
  prim args lhs rule type_rule macro_rule simple_rules simple_rulel simple_rule
%printer { debug_stream() << $$->r; } rhs qual_rhs
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
| source item_pos
| error ';'
{ interp.nerrs = yyerrstatus_ = 0; interp.declare_op = false;
  interp.symtab.clean_namespaces(); }
;

/* Same as above for namespace scopes. This doesn't allow empty items. */

items
: item_pos
| items item_pos
| error ';'
{ interp.nerrs = yyerrstatus_ = 0; interp.declare_op = false;
  interp.symtab.clean_namespaces(); }
;

item_pos
: { interp.line = yylloc.begin.line; interp.column = yylloc.begin.column; }
  item
;

item
: expr ';'
{ interp.loc = &yyloc;
  if (interp.tags<0) { restricted_action(interp.exec($1, true), delete $1); }
  else if (!interp.tags) { restricted_action(interp.exec($1), delete $1); }
  else delete $1; }
| ESCAPE expr EOX
{ interp.loc = &yyloc;
  if (!interp.tags) { restricted_action(interp.parse($2), delete $2); }
  else delete $2;
  // We only parse a single expression in this mode, bail out.
  if (yychar > 0 && interp.nerrs == 0)
    error(yylloc, "syntax error, expected end of file");
  YYACCEPT; }
| LET simple_rule ';'
{ interp.loc = &yyloc; action(interp.define($2), delete $2); }
| CONST simple_rule ';'
{ interp.loc = &yyloc; action(interp.define_const($2), delete $2); }
| DEF macro_rule ';'
{ interp.loc = &yyloc; action(interp.add_macro_rules($2), delete $2); }
| INTERFACE ID
{ expr *id = 0;
  string *s = new string(*$2);
  try { id = interp.mksym_expr(s); }
  catch (err &e) {
    interp.error(yyloc, e.what());
  }
  if (id && id->is_fun()) {
    // This will promote the symbol to the proper namespace if it hasn't been
    // declared yet.
    interp.checkvars(*id);
    interp.symtab.sym(id->tag()).unresolved = false;
    interp.typeenv[id->tag()];
    $<ival>$ = id->tag();
  } else {
    error(yylloc, "error in interface declaration (invalid type identifier)");
    $<ival>$ = 0;
  }
  if (id) delete id;
}
WITH { $<ival>$ = $<ival>3; } interface_rules END ';'
{ int32_t tag = $<ival>3, count = $6;
  delete $2;
  if (tag) interp.finalize_interface_rules(interp.typeenv, tag, count);
}
| TYPE type_rule ';'
{ interp.loc = &yyloc;
  action(interp.add_type_rules(interp.typeenv, $2), delete $2); }
| rule ';'
{ interp.loc = &yyloc;
  rulel *rl = 0;
  bool headless = $1->front().lhs.is_null();
  action(interp.add_rules(interp.globenv,
  (rl = interp.default_lhs(interp.last, $1)), headless, true),
  if (rl) delete rl); }
| fixity
{ if ($1->special && $1->fix != nonfix && $1->fix != outfix &&
      $1->prec >= PREC_MAX) {
    error(yylloc, "invalid fixity declaration"); $1->prec = 0;
  } }
  ids ';'
{ interp.declare_op = false;
  action(interp.declare($1->priv, $1->prec, $1->fix, $3), );
  if (interp.tags) interp.add_tags($3);
  delete $3; delete $1; }
| USING fnames ';'
{ action(interp.run(*$2), {}); delete $2; }
| USING scope { interp.declare_op = false; } fnames ';'
{ action(interp.run($2, *$4), {}); delete $4; }
| NAMESPACE name opt_brackets ';'
{ if (interp.active_namespaces.empty()) {
    action(interp.set_namespace($2, $3), );
  } else
    error(yylloc, "namespace declaration is not permitted here");
}
| NAMESPACE opt_brackets ';'
{ if (interp.active_namespaces.empty()) {
    action(interp.clear_namespace($2), );
  } else
    error(yylloc, "namespace declaration is not permitted here");
}
| NAMESPACE name opt_brackets WITH
{ action(interp.push_namespace(new string(*$2), $3), ); }
  items END ';'
{ interp.pop_namespace(); delete $2; }
| USING NAMESPACE name_xsyms_list ';'
{ action(interp.using_namespaces($3), ); }
| USING NAMESPACE ';'
{ action(interp.using_namespaces(), ); }
| scope EXTERN { interp.declare_op = false; extern_priv = $1; } prototypes ';'
| EXTERN { extern_priv = -1; } prototypes ';'
| CODE
{ action(interp.inline_code(false, interp.xcode), {}); interp.xcode.clear(); }
| scope { interp.declare_op = false; } CODE
{ action(interp.inline_code($1, interp.xcode), {}); interp.xcode.clear(); }
;

/* Lexical tie-in: We need to tell the lexer that we're defining new operator
   symbols (interp.declare_op = true) instead of searching for existing ones
   in the symbol table. */

fixity
: FIX INT		{ $$ = new sym_info(true, priv_def,$2,$1);
			  interp.declare_op = true; }
| FIX BIGINT		{ $$ = new sym_info(true, priv_def,mpz_get_si(*$2),$1);
			  free($2); interp.declare_op = true; }
| FIX '(' op ')'	{ symbol& sym = interp.symtab.sym($3);
			  $$ = new sym_info(true, priv_def,sym.prec,$1);
			  interp.declare_op = true; }
| OUTFIX		{ $$ = new sym_info(true, priv_def,PREC_MAX,outfix);
			  interp.declare_op = true; }
| NONFIX		{ $$ = new sym_info(true, priv_def,PREC_MAX,nonfix);
			  interp.declare_op = true; }
| scope FIX INT		{ $$ = new sym_info(true, $1,$3,$2);
			  interp.declare_op = true; }
| scope FIX BIGINT	{ $$ = new sym_info(true, $1,mpz_get_si(*$3),$2);
			  free($3); interp.declare_op = true; }
| scope FIX '(' op ')'	{ symbol& sym = interp.symtab.sym($4);
			  $$ = new sym_info(true, $1,sym.prec,$2);
			  interp.declare_op = true; }
| scope OUTFIX		{ $$ = new sym_info(true, $1,PREC_MAX,outfix); }
| scope NONFIX		{ $$ = new sym_info(true, $1,PREC_MAX,nonfix); }
| scope			{ $$ = new sym_info(false, $1,PREC_MAX,infix); }
;

op
: NA			{ $$ = $1->tag(); delete $1; }
| LT			{ $$ = $1->tag(); delete $1; }
| RT			{ $$ = $1->tag(); delete $1; }
| PR			{ $$ = $1->tag(); delete $1; }
| PO			{ $$ = $1->tag(); delete $1; }
;

scope
: PUBLIC		{ $$ = false; interp.declare_op = true; }
| PRIVATE		{ $$ = true;  interp.declare_op = true; }
;

ids
: ID
{ $$ = new list<string>; $$->push_back(*$1); delete $1; }
| ids ID
{ $$ = $1; $$->push_back(*$2); delete $2; }
;

fnames
: fname
{ $$ = new list<string>; $$->push_back(*$1); delete $1; }
| fnames ',' fname
{ $$ = $1; $$->push_back(*$3); delete $3; }
;

fname
: ID			{ $$ = $1; mangle_fname(*$$); }
| STR			{ char *s = fromutf8($1); free($1);
			  $$ = new string(s); free(s); }
;

opt_brackets
: /* empty */		{ $$ = 0; }
| '(' { interp.xsym_prefix = 0; } XID XID ')'
			{ int32_t g = interp.symtab.sym($3).g;
			  if (g != 0 && g == $4)
			    $$ = $3;
			  else {
			    string msg;
			    if (g == 0) {
			      string id = interp.symtab.sym($3).s;
			      msg = "syntax error, unexpected '"+id+
				"', expecting outfix operator";
			    } else {
			      string id = interp.symtab.sym($4).s;
			      string rid = interp.symtab.sym(g).s;
			      msg = "syntax error, unexpected '"+id+
				"', expecting '"+rid+"'";
			    }
			    interp.error(yyloc, msg);
			    YYERROR;
			  } }
;

name_xsyms_list
: name_xsyms
{ $$ = new list< pair< string, list<int32_t> > >;
  $$->push_back(*$1); delete $1; }
| name_xsyms_list ',' name_xsyms
{ $$ = $1; $$->push_back(*$3); delete $3; }
;

name_xsyms
: name
{ $$ = new pair< string, list<int32_t> >(*$1, list<int32_t>()); delete $1; }
| name '(' { interp.xsym_prefix = $1; } xsyms ')'
{ $$ = new pair< string, list<int32_t> >(*$1, *$4); delete $1; delete $4; }
;

xsyms
: XID
{ $$ = new list<int32_t>; $$->push_back($1); }
| xsyms XID
{ $$ = $1; $$->push_back($2); }
;

name
: ID
{ $$ = $1; if ($$->compare(0, 2, "::") == 0) $$->erase(0, 2); }
| STR
{ $$ = new string($1); free($1);
  if ($$->compare(0, 2, "::") == 0) $$->erase(0, 2); }
;

prototypes
: prototype
| prototypes ',' prototype
;

prototype
: ctype ID '(' opt_ctypes ')' optalias
{ action(interp.declare_extern(extern_priv, *$2, *$1, $4->second, $4->first,
			       0, *$6), {});
  if (interp.tags) interp.add_tags(*$2, *$6);
  delete $1; delete $2; delete $4; delete $6; }
;

opt_ctypes
: /* empty */
{ $$ = new pair < bool, list<string> >; $$->first = false; }
| ELLIPSIS
{ $$ = new pair < bool, list<string> >; $$->first = true; }
| ctypes
| ctypes ',' ELLIPSIS
{ $$ = $1; $$->first = true; }
;

ctypes
: ctype optname
{ $$ = new pair < bool, list<string> >; $$->second.push_back(*$1); delete $1; }
| ctypes ',' ctype optname
{ $$ = $1; $$->second.push_back(*$3); delete $3; }
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
: simple
{ parser_action($$ = interp.mksimple_expr($1), delete $1); }
| '\\' args MAPSTO expr
{ try { $$ = interp.mklambda_expr($2, $4); }
  catch (err &e) { interp.error(yyloc, e.what()); $$ = new dummy_expr; } }
| CASE expr OF pat_rules END
{ try { $$ = interp.mkcase_expr($2, new rulel($4->rl)); delete $4; }
  catch (err &e) { interp.error(yyloc, e.what()); delete $4; $$ = new dummy_expr; } }
| expr WHEN simple_rules END
{ try { $$ = interp.mkwhen_expr($1, $3); }
  catch (err &e) { interp.error(yyloc, e.what()); $$ = new dummy_expr; } }
| expr WITH rules END
{ try { $$ = interp.mkwith_expr($1, new env($3->e)); delete $3; }
  catch (err &e) { interp.error(yyloc, e.what()); delete $3; $$ = new dummy_expr; } }
| IF expr THEN expr ELSE expr
{ $$ = interp.mkcond_expr($2, $4, $6); }
;

args
: prim
{ $$ = new exprl; $$->push_back(*$1); delete $1; }
| args prim
{ $$ = $1; $$->push_back(*$2); delete $2; }
;

/* Simple expressions (infix, prefix, postfix, applications). These are parsed
   as a sequence of primaries here, which is then rearranged using a separate
   operator precedence parser. This enables us to deal with an unlimited
   number of precedence levels. */

simple
: prim			{ $$ = (new OpStack())->push_arg($1); }
| PR			{ $$ = (new OpStack())->push_op($1); }
| simple prim		{ $$ = $1->push_arg($2); }
| simple PO		{ $$ = $1->push_op($2); }
| simple PR		{ $$ = $1->push_op($2); }
| simple LT		{ $$ = $1->push_op($2); }
| simple RT		{ $$ = $1->push_op($2); }
| simple NA		{ $$ = $1->push_op($2); }
;

/* Primary expressions. */

prim
: ID			{ try { $$ = interp.mksym_expr($1); }
			  catch (err &e) {
			    interp.error(yyloc, e.what());
			    $$ = new dummy_expr;
			  } }
| ID TAG		{ try { $$ = interp.mksym_expr($1, $2); }
			  catch (err &e) {
			    interp.error(yyloc, e.what());
			    $$ = new dummy_expr;
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
| LO expr RO		{ int32_t g = interp.symtab.sym($1->tag()).g;
			  assert(g != 0);
			  if (g == $3->tag()) {
			    if (interp.symtab.sym($1->tag()).ns) {
			      // special namespace bracket
			      delete $1;
			      $$ = $2;
			      interp.symtab.pop_namespace();
			    } else
			      $$ = interp.mkexpr($1, $2);
			    delete $3;
			  } else {
			    string id = interp.symtab.sym($3->tag()).s;
			    string rid = interp.symtab.sym(g).s;
			    string msg = "syntax error, unexpected '"+id+
			      "', expecting '"+rid+"'";
			    interp.error(yyloc, msg);
			    YYERROR;
			  } }
| '{' rows '}'		{ $$ = new expr(EXPR::MATRIX, $2); }
| '{' expr '|' comp_clauses '}'
			{ try { $$ = interp.mkmatcomp_expr($2, $4); }
			  catch (err &e) {
			    interp.error(yyloc, e.what());
			    $$ = new dummy_expr;
			  } }
| '[' expr ']'		{ $$ = interp.mklist_expr($2); }
| '[' ']'		{ $$ = new expr(interp.symtab.nil_sym().f); }
| '[' expr '|' comp_clauses ']'
			{ try { $$ = interp.mklistcomp_expr($2, $4); }
			  catch (err &e) {
			    interp.error(yyloc, e.what());
			    $$ = new dummy_expr;
			  } }
| '(' expr ')'		{ $$ = $2;
			  if ($$->is_pair()) $$->flags() |= EXPR::PAREN; }
| '(' ')'		{ $$ = new expr(interp.symtab.void_sym().f); }
| '(' NA ')'		{ $$ = $2; }
| '(' LT ')'		{ $$ = $2; }
| '(' RT ')'		{ $$ = $2; }
| '(' PR ')'		{ $$ = $2; }
| '(' PO ')'		{ $$ = $2; }
| '(' LO RO ')'		{ int32_t g = interp.symtab.sym($2->tag()).g;
			  assert(g != 0);
			  if (g == $3->tag()) {
			    $$ = $2;
			    delete $3;
			  } else {
			    string id = interp.symtab.sym($3->tag()).s;
			    string rid = interp.symtab.sym(g).s;
			    string msg = "syntax error, unexpected '"+id+
			      "', expecting '"+rid+"'";
			    interp.error(yyloc, msg);
			    YYERROR;
			  } }

/* Left sections. These cause a number of shift/reduce conflicts with the
   rules for simple above. */

| '(' simple NA ')'
{ expr *x; parser_action(x = interp.mksimple_expr($2),
			 (delete $2, delete $3));
  $$ = interp.mklsect($3, x); }
| '(' simple LT ')'
{ expr *x; parser_action(x = interp.mksimple_expr($2),
			 (delete $2, delete $3));
  $$ = interp.mklsect($3, x); }
| '(' simple RT ')'
{ expr *x; parser_action(x = interp.mksimple_expr($2),
			 (delete $2, delete $3));
  $$ = interp.mklsect($3, x); }
| '(' simple PR ')'
{ expr *x; parser_action(x = interp.mksimple_expr($2),
			 (delete $2, delete $3));
  $$ = interp.mklsect($3, x); }

/* Right sections. */

| '(' NA simple ')'
{ expr *x; parser_action(x = interp.mksimple_expr($3),
			 (delete $2, delete $3));
  $$ = interp.mkrsect($2, x); }
| '(' LT simple ')'
{ expr *x; parser_action(x = interp.mksimple_expr($3),
			 (delete $2, delete $3));
  $$ = interp.mkrsect($2, x); }
| '(' RT simple ')'
{ expr *x; parser_action(x = interp.mksimple_expr($3),
			 (delete $2, delete $3));
  $$ = interp.mkrsect($2, x); }

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

/* Rewriting rule syntax. These generally take the form l = r [if g]; ... For
   convenience, we also allow a semicolon at the end of a rule list. Moreover,
   multiple left-hand sides are permitted (denoting a collection of rules for
   the same right-hand side), and the left-hand side may also be omitted, in
   which case the left-hand sides of the previous rule are repeated. */

rule
: lhs '=' rhs
{ $$ = new rulel;
  for (exprl::iterator l = $1->begin(), end = $1->end(); l != end; l++)
    $$->push_back(rule(*l, $3->rhs(), $3->qual()));
  delete $1; delete $3; }
| '=' rhs
{ $$ = new rulel(1, rule(expr(), $2->rhs(), $2->qual())); delete $2; }
;

lhs
: expr
{ $$ = new exprl; $$->push_back(*$1); delete $1; }
| lhs '|' expr
{ $$ = $1; $$->push_back(*$3); delete $3; }
;

rhs
: expr			{ $$ = new rhs_info($1); }
| qual_rhs
;

qual_rhs
: expr OTHERWISE	{ $$ = new rhs_info($1); }
| expr IF simple
{ expr *x; parser_action(x = interp.mksimple_expr($3),
			 (delete $1, delete $3));
  $$ = new rhs_info($1, x); }
| qual_rhs WHEN simple_rules END
{ $$ = $1;
  if ($$->q) {
    $$->r = interp.mkcond1_expr($$->q, $$->r); $$->q = 0;
  }
  try {
    expr *x = interp.mkwhen_expr($$->r, $3);
    $$->r = x;
  } catch (err &e) {
    interp.error(yyloc, e.what());
  }
}
| qual_rhs WITH rules END
{ $$ = $1;
  if ($$->q) {
    $$->r = interp.mkcond1_expr($$->q, $$->r); $$->q = 0;
  }
  try {
    expr *x = interp.mkwith_expr($$->r, new env($3->e)); delete $3;
    $$->r = x;
  } catch (err &e) {
    interp.error(yyloc, e.what());
  }
}
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
| expr
{ expr *x = new expr(interp.symtab.anon_sym);
  $$ = new rule(*x, *$1); delete x; delete $1; }
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

/* Same for type rules (this is only used at the toplevel). These have the
   same form as normal rules in function definitions, minus the headless
   (continuation) rules. Also, the rhs may be omitted in which case it
   defaults to 'true'. */

type_rule
: lhs
{ $$ = new rulel;
  for (exprl::iterator l = $1->begin(), end = $1->end(); l != end; l++) {
    if (l->is_fun()) {
      // Just declare the symbol so that the compiler knows about it.
      // This will promote the symbol to the proper namespace if it hasn't
      // been declared yet.
      interp.checkvars(*l);
      interp.symtab.sym(l->tag()).unresolved = false;
      if ((interp.verbose&verbosity::defs) != 0)
	cout << "type " << *l << ";\n";
      interp.typeenv[l->tag()];
    } else
      // assume rhs = true
      $$->push_back(rule(*l, expr(EXPR::INT, 1)));
  }
  delete $1; }
| lhs '=' rhs
{ $$ = new rulel;
  for (exprl::iterator l = $1->begin(), end = $1->end(); l != end; l++)
    $$->push_back(rule(*l, $3->rhs(), $3->qual()));
  delete $1; delete $3; }
;

/* Same for macro rules. These have the same form as normal rules in function
   definitions, minus the guards and the headless (continuation) rules. */

macro_rule
: lhs '=' expr
{ $$ = new rulel;
  for (exprl::iterator l = $1->begin(), end = $1->end(); l != end; l++)
    $$->push_back(rule(*l, *$3));
  delete $1; delete $3; }
;

/* Interface type definition. This is simply a semicolon-delimited collection
   of patterns specifying the operations that should be defined on the type.
   The whole section can also be empty (which means that any expression
   matches the type). Otherwise each pattern must follow the syntactic rules
   of the lhs of a function definition, so that the section is like a
   collection of rules without right-hand sides and qualifiers. */

interface_rules
: /* empty */
{ $$ = 0; }
| interface_rules expr
{ interp.loc = &yyloc;
  int32_t tag = $<ival>0, count = 0;
  if (tag) {
    action((interp.add_interface_rule(interp.typeenv, tag, *$2), count++), );
  }
  $<ival>$ = count;
}
';'
{ $$ = $1+$<ival>3; delete $2; }
| interface_rules INTERFACE ID
{ const symbol &sym = interp.symtab.checksym(*$3);
  interp.loc = &yyloc;
  int32_t tag = $<ival>0, iface = sym.f, count = 0;
  if (tag) {
    action((count = interp.add_sub_interface(interp.typeenv, tag, iface)), );
  }
  $<ival>$ = count;
}
';'
{ $$ = $1+$<ival>4; delete $3; }
;

%%

void
yy::parser::error (const yy::parser::location_type& l,
		   const string& m)
{
  interp.error(l, m);
}

static void mangle_fname(string& name)
{
  size_t pos = name.find("::");
  while (pos != string::npos) {
    name.replace(pos, 2, "/");
    pos = name.find("::", pos);
  }
  name += ".pure";
}
