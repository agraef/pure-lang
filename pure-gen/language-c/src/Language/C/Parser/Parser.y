-----------------------------------------------------------------------------
-- Module      :  Parser.y
-- Copyright   :  (c) 2005-2007 Duncan Coutts
--                (c) 2008 Benedikt Huber
--                (c) [1999..2004] Manuel M T Chakravarty
--                Portions copyright 1989, 1990 James A. Roskind
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
--  Parser for C translation units, which have already been run through the C
--  preprocessor. It is recommended to use the `strict' flag for happy.
--
--  The parser recognizes all of ISO C 99 and most GNU C extensions.
--
--  With C99 we refer to the ISO C99 standard, specifically the section numbers
--  used below refer to this report:
--
--    <http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf>
--
-- GNU extensions are documented in the gcc parser
--
--    <http://gcc.gnu.org/viewcvs/trunk/gcc/c-parser.c>
--
-- and in: <http://gcc.gnu.org/onlinedocs/gcc/C-Extensions.html>
--
-- The set of supported extensions is documented in
--
--    <http://www.sivity.net/projects/language.c/wiki/Cee>
------------------------------------------------------------------
{
module Language.C.Parser.Parser (
  -- * Parse a C translation unit
  parseC,
  -- * Exposed Parsers
  translUnitP, extDeclP, statementP, expressionP
) where

-- Relevant C99 sections:
--
-- 6.5 Expressions .1 - .17 and 6.6 (almost literally)
--  Supported GNU extensions:
--     - Allow a compound statement as an expression
--     - Various __builtin_* forms that take type parameters
--     - `alignof' expression or type
--     - `__extension__' to suppress warnings about extensions
--     - Allow taking address of a label with: && label
--     - Omitting the `then' part of conditional expressions
--     - complex numbers
--
-- 6.7 C Declarations .1 -.8
--  Supported GNU extensions:
--     - '__thread' thread local storage (6.7.1)
--
-- 6.8 Statements .1 - .8
--  Supported GNU extensions:
--    - case ranges (C99 6.8.1)
--    - '__label__ ident;' declarations (C99 6.8.2)
--    - computed gotos (C99 6.8.6)
--
-- 6.9 Translation unit
--  Supported GNU extensions:
--     - allow empty translation_unit
--     - allow redundant ';'
--     - allow extension keyword before external declaration
--     - asm definitions
--
--  Since some of the grammar productions are quite difficult to read,
--  (especially those involved with the decleration syntax) we document them
--  with an extended syntax that allows a more consise representation:
--
--  Ordinary rules
--
--   foo      named terminal or non-terminal
--
--   'c'      terminal, literal character token
--
--   A B      concatenation
--
--   A | B    alternation
--
--   (A)      grouping
--
--  Extended rules
--
--   A?       optional, short hand for (A|) or [A]{ 0==A || 1==A }
--
--   ...      stands for some part of the grammar omitted for clarity
--
--   {A}      represents sequences, 0 or more.
--
--   <permute> modifier which states that any permutation of the immediate subterms is valid
--
--
--- TODO ----------------------------------------------------------------------
--
--  !* We ignore the C99 static keyword (see C99 6.7.5.3)
--  !* We do not distinguish in the AST between incomplete array types and
--      complete variable length arrays ([ '*' ] means the latter). (see C99 6.7.5.2)
--  !* The AST doesn't allow recording __attribute__ of unnamed struct field
--     (see , struct_default_declaring_list, struct_identifier_declarator)
--  !* see `We're being far to liberal here' (... struct definition within structs)
--  * Documentation isn't complete and consistent yet.

import Prelude    hiding (reverse)
import qualified Data.List as List
import Control.Monad (mplus)
import Language.C.Parser.Builtin   (builtinTypeNames)
import Language.C.Parser.Lexer     (lexC, parseError)
import Language.C.Parser.Tokens    (CToken(..), GnuCTok(..), posLenOfTok)
import Language.C.Parser.ParserMonad (P, failP, execParser, getNewName, addTypedef, shadowTypedef, getCurrentPosition,
                                      enterScope, leaveScope, getLastToken, getSavedToken, ParseError(..))

import Language.C.Data.RList
import Language.C.Data.InputStream
import Language.C.Data.Ident
import Language.C.Data.Name
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Syntax

}
-- in order to document the parsers, we have to alias them
%name translation_unit translation_unit
%name external_declaration external_declaration
%name statement statement
%name expression expression

%tokentype { CToken }

%monad { P } { >>= } { return }
%lexer { lexC } { CTokEof }

%expect 1

%token

'('		{ CTokLParen	_ }
')'		{ CTokRParen	_ }
'['		{ CTokLBracket	_ }
']'		{ CTokRBracket	_ }
"->"		{ CTokArrow	_ }
'.'		{ CTokDot	_ }
'!'		{ CTokExclam	_ }
'~'		{ CTokTilde	_ }
"++"		{ CTokInc	_ }
"--"		{ CTokDec	_ }
'+'		{ CTokPlus	_ }
'-'		{ CTokMinus	_ }
'*'		{ CTokStar	_ }
'/'		{ CTokSlash	_ }
'%'		{ CTokPercent	_ }
'&'		{ CTokAmper	_ }
"<<"		{ CTokShiftL	_ }
">>"		{ CTokShiftR	_ }
'<'		{ CTokLess	_ }
"<="		{ CTokLessEq	_ }
'>'		{ CTokHigh	_ }
">="		{ CTokHighEq	_ }
"=="		{ CTokEqual	_ }
"!="		{ CTokUnequal	_ }
'^'		{ CTokHat	_ }
'|'		{ CTokBar	_ }
"&&"		{ CTokAnd	_ }
"||"		{ CTokOr	_ }
'?'		{ CTokQuest	_ }
':'		{ CTokColon	_ }
'='		{ CTokAssign	_ }
"+="		{ CTokPlusAss	_ }
"-="		{ CTokMinusAss	_ }
"*="		{ CTokStarAss	_ }
"/="		{ CTokSlashAss	_ }
"%="		{ CTokPercAss	_ }
"&="		{ CTokAmpAss	_ }
"^="		{ CTokHatAss	_ }
"|="		{ CTokBarAss	_ }
"<<="		{ CTokSLAss	_ }
">>="		{ CTokSRAss	_ }
','		{ CTokComma	_ }
';'		{ CTokSemic	_ }
'{'		{ CTokLBrace	_ }
'}'		{ CTokRBrace	_ }
"..."		{ CTokEllipsis	_ }
alignof		{ CTokAlignof	_ }
asm		{ CTokAsm	_ }
auto		{ CTokAuto	_ }
break		{ CTokBreak	_ }
"_Bool"		{ CTokBool	_ }
case		{ CTokCase	_ }
char		{ CTokChar	_ }
const		{ CTokConst	_ }
continue	{ CTokContinue	_ }
"_Complex"	{ CTokComplex	_ }
default		{ CTokDefault	_ }
do		{ CTokDo	_ }
double		{ CTokDouble	_ }
else		{ CTokElse	_ }
enum		{ CTokEnum	_ }
extern		{ CTokExtern	_ }
float		{ CTokFloat	_ }
"__float128"	{ CTokFloat128	_ }
"_Float128"	{ CTokFloat128	_ }
for		{ CTokFor	_ }
goto		{ CTokGoto	_ }
if		{ CTokIf	_ }
inline		{ CTokInline	_ }
int		{ CTokInt	_ }
"__int128"  { CTokInt128 _ }
long		{ CTokLong	_ }
"__label__"	{ CTokLabel	_ }
register	{ CTokRegister	_ }
restrict	{ CTokRestrict	_ }
return		{ CTokReturn	_ }
short		{ CTokShort	_ }
signed		{ CTokSigned	_ }
sizeof		{ CTokSizeof	_ }
static		{ CTokStatic	_ }
struct		{ CTokStruct	_ }
switch		{ CTokSwitch	_ }
typedef		{ CTokTypedef	_ }
typeof		{ CTokTypeof	_ }
"__thread"	{ CTokThread	_ }
union		{ CTokUnion	_ }
unsigned	{ CTokUnsigned	_ }
void		{ CTokVoid	_ }
volatile	{ CTokVolatile	_ }
while		{ CTokWhile	_ }
cchar		{ CTokCLit   _ _ }		-- character constant
cint		{ CTokILit   _ _ }		-- integer constant
cfloat		{ CTokFLit   _ _ }		-- float constant
cstr		{ CTokSLit   _ _ }		-- string constant (no escapes)
ident		{ CTokIdent  _ $$ }		-- identifier
tyident		{ CTokTyIdent _ $$ }		-- `typedef-name' identifier
"__attribute__"	{ CTokGnuC GnuCAttrTok _ }	-- special GNU C tokens
"__extension__"	{ CTokGnuC GnuCExtTok  _ }	-- special GNU C tokens
"__real__"        { CTokGnuC GnuCComplexReal _ }
"__imag__"        { CTokGnuC GnuCComplexImag _ }
-- special GNU C builtin 'functions' that actually take types as parameters:
"__builtin_va_arg"		{ CTokGnuC GnuCVaArg    _ }
"__builtin_offsetof"		{ CTokGnuC GnuCOffsetof _ }
"__builtin_types_compatible_p"	{ CTokGnuC GnuCTyCompat _ }

%%


-- parse a complete C translation unit
-- we have to take special care of empty translation units
translation_unit :: { CTranslUnit }
translation_unit
  : ext_decl_list	{% let decls = reverse $1 in
                       case decls of
                           []     -> do{ n <- getNewName; p <- getCurrentPosition; return $ CTranslUnit decls (mkNodeInfo' p (p,0) n) }
                           (d:ds) -> withNodeInfo d $ CTranslUnit decls }


-- parse a list of external declarations, making up a C translation unit (C99 6.9)
--
-- * GNU extensions:
--     allow empty translation_unit
--     allow redundant ';'
--
ext_decl_list :: { Reversed [CExtDecl] }
ext_decl_list
  : {- empty -}					        { empty }
  | ext_decl_list ';'			        { $1 }
  | ext_decl_list external_declaration	{ $1 `snoc` $2 }


-- parse external C declaration (C99 6.9)
--
-- * GNU extensions:
--     allow extension keyword before external declaration
--     asm definitions
external_declaration :: { CExtDecl }
external_declaration
  : function_definition		              { CFDefExt $1 }
  | declaration			                  { CDeclExt $1 }
  | "__extension__" external_declaration  { $2 }
  | asm '(' string_literal ')' ';'		  {% withNodeInfo $1 $ CAsmExt $3 }


-- parse C function definition (C99 6.9.1)
--
-- function_definition :- specifiers? fun-declarator compound-statement
--                        specifiers? old-fun-declarator  declaration-list compound-statement
--
-- The specifiers are a list consisting of type-names (int, struct foo, ...),
-- storage-class specifiers (extern, static,...) and type qualifiers (const, volatile, ...).
--
--   declaration_specifier      :- <permute> type-qualifier* storage-class+ typename+    "extern unsigned static volatile int f()"
--   type_specifier             :- <permute> type-qualifier* typename+                   "const int f()", "long int f()"
--   declaration_qualifier_list :- <permute> type_qualifier* storage-class+              "extern static const f()"
--   type_qualifier_list        :- type-qualifier+                                       "const f()"
--
-- * GNU extension:
--    __attribute__ annotations
--
function_definition :: { CFunDef }
function_definition
  :                            function_declarator compound_statement
  	{% leaveScope >> (withNodeInfo $1 $ CFunDef [] $1 [] $2) }

  |                      attrs function_declarator compound_statement
    {% leaveScope >> (withNodeInfo $1 $ CFunDef (liftCAttrs $1) $2 [] $3) }

  | declaration_specifier      function_declarator compound_statement
	  {% leaveScope >> (withNodeInfo $1 $ CFunDef $1 $2 [] $3) }

  | type_specifier             function_declarator compound_statement
	  {% leaveScope >> (withNodeInfo $1 $ CFunDef $1 $2 [] $3) }

  | declaration_qualifier_list function_declarator compound_statement
	  {% leaveScope >> (withNodeInfo $1 $ CFunDef (reverse $1) $2 [] $3) }

  | type_qualifier_list   function_declarator compound_statement
	  {% leaveScope >> (withNodeInfo $1 $ CFunDef (liftTypeQuals $1) $2 [] $3) }

  | type_qualifier_list   attrs function_declarator compound_statement
	  {% leaveScope >> (withNodeInfo $1 $ CFunDef (liftTypeQuals $1 ++ liftCAttrs $2) $3 [] $4) }

  -- old function declarators

  |                            function_declarator_old declaration_list compound_statement
  	{% withNodeInfo $1 $ CFunDef [] $1 (reverse $2) $3 }

  |                      attrs function_declarator_old declaration_list compound_statement
  	{% withNodeInfo $2 $ CFunDef (liftCAttrs $1) $2 (reverse $3) $4 }

  | declaration_specifier      function_declarator_old declaration_list compound_statement
  	{% withNodeInfo $1 $ CFunDef $1 $2 (reverse $3) $4 }

  | type_specifier             function_declarator_old declaration_list compound_statement
  	{% withNodeInfo $1 $ CFunDef $1 $2 (reverse $3) $4 }

  | declaration_qualifier_list function_declarator_old declaration_list compound_statement
  	{% withNodeInfo $1 $ CFunDef (reverse $1) $2 (reverse $3) $4 }

  | type_qualifier_list   function_declarator_old declaration_list compound_statement
  	{% withNodeInfo $1 $ CFunDef (liftTypeQuals $1) $2 (reverse $3) $4 }

  | type_qualifier_list attrs  function_declarator_old declaration_list compound_statement
  	{% withNodeInfo $1 $ CFunDef (liftTypeQuals $1  ++ liftCAttrs $2) $3 (reverse $4) $5 }

-- Read declarator and put function
function_declarator :: { CDeclr }
function_declarator
  : identifier_declarator
  	{% let declr = reverseDeclr $1 in
  	   enterScope >> doFuncParamDeclIdent declr >> return declr }


-- parse C statement (C99 6.8)
--
-- * GNU extension: ' __asm__ (...); ' statements
--
statement :: { CStat }
statement
  : labeled_statement			{ $1 }
  | compound_statement		{ $1 }
  | expression_statement	{ $1 }
  | selection_statement		{ $1 }
  | iteration_statement		{ $1 }
  | jump_statement			  { $1 }
  | asm_statement			    {% withNodeInfo $1 (CAsm $1) }


-- parse C labeled statement (C99 6.8.1)
--
-- * GNU extension: case ranges
--
labeled_statement :: { CStat }
labeled_statement
  : identifier ':' attrs_opt statement		{% withNodeInfo $1 $ CLabel $1 $4 $3 }
  | case constant_expression ':' statement	{% withNodeInfo $1 $ CCase $2 $4 }
  | default ':' statement			{% withNodeInfo $1 $ CDefault $3 }
  | case constant_expression "..." constant_expression ':' statement
  	{% withNodeInfo $1 $ CCases $2 $4 $6 }


-- parse C compound statement (C99 6.8.2)
--
-- * GNU extension: '__label__ ident;' declarations
--
compound_statement :: { CStat }
compound_statement
  : '{' enter_scope block_item_list leave_scope '}'
  	{% withNodeInfo $1 $ CCompound [] (reverse $3) }

  | '{' enter_scope label_declarations block_item_list leave_scope '}'
  	{% withNodeInfo $1 $ CCompound (reverse $3) (reverse $4) }


-- No syntax for these, just side effecting semantic actions.
--
enter_scope :: { () }
enter_scope : {% enterScope }
leave_scope :: { () }
leave_scope : {% leaveScope }


block_item_list :: { Reversed [CBlockItem] }
block_item_list
  : {- empty -}			{ empty }
  | block_item_list block_item	{ $1 `snoc` $2 }

block_item :: { CBlockItem }
block_item
  : statement			{ CBlockStmt $1 }
  | nested_declaration		{ $1 }

nested_declaration :: { CBlockItem }
nested_declaration
  : declaration				{ CBlockDecl $1 }
  | nested_function_definition		{ CNestedFunDef $1 }
  | "__extension__" nested_declaration	{ $2 }

nested_function_definition :: { CFunDef }
nested_function_definition
  : declaration_specifier      function_declarator compound_statement
	{% leaveScope >> (withNodeInfo $1 $ CFunDef $1 $2 [] $3) }

  | type_specifier             function_declarator compound_statement
	{% leaveScope >> (withNodeInfo $1 $ CFunDef $1 $2 [] $3) }

  | declaration_qualifier_list function_declarator compound_statement
	{% leaveScope >> (withNodeInfo $1 $ CFunDef (reverse $1) $2 [] $3) }

  | type_qualifier_list   function_declarator compound_statement
	{% leaveScope >> (withNodeInfo $1 $ CFunDef (liftTypeQuals $1) $2 [] $3) }

  | type_qualifier_list   attrs function_declarator compound_statement
	{% leaveScope >> (withNodeInfo $1 $ CFunDef (liftTypeQuals $1 ++ liftCAttrs $2) $3 [] $4) }


label_declarations :: { Reversed [Ident] }
label_declarations
  : "__label__" identifier_list ';'			{ $2  } --TODO
  | label_declarations "__label__" identifier_list ';'	{ $1 `rappendr` $3 }


-- parse C expression statement (C99 6.8.3)
--
expression_statement :: { CStat }
expression_statement
  : ';'				{% withNodeInfo $1 $ CExpr Nothing }
  | expression ';'		{% withNodeInfo $1 $ CExpr (Just $1) }


-- parse C selection statement (C99 6.8.4)
--
selection_statement :: { CStat }
selection_statement
  : if '(' expression ')' statement
	{% withNodeInfo $1 $ CIf $3 $5 Nothing }

  | if '(' expression ')' statement else statement
	{% withNodeInfo $1 $ CIf $3 $5 (Just $7) }

  | switch '(' expression ')' statement
	{% withNodeInfo $1 $ CSwitch $3 $5 }


-- parse C iteration statement (C99 6.8.5)
--
iteration_statement :: { CStat }
iteration_statement
  : while '(' expression ')' statement
  	{% withNodeInfo $1 $ CWhile $3 $5 False }

  | do statement while '(' expression ')' ';'
  	{% withNodeInfo $1 $ CWhile $5 $2 True }

  | for '(' expression_opt ';' expression_opt ';' expression_opt ')' statement
	{% withNodeInfo $1 $ CFor (Left $3) $5 $7 $9 }

  | for '(' enter_scope declaration expression_opt ';' expression_opt ')' statement leave_scope
	{% withNodeInfo $1 $ CFor (Right $4) $5 $7 $9 }


-- parse C jump statement (C99 6.8.6)
--
-- * GNU extension: computed gotos
--
jump_statement :: { CStat }
jump_statement
  : goto identifier ';'			{% withNodeInfo $1 $ CGoto $2 }
  | goto '*' expression ';'		{% withNodeInfo $1 $ CGotoPtr $3 }
  | continue ';'			    {% withNodeInfo $1 $ CCont }
  | break ';'				    {% withNodeInfo $1 $ CBreak }
  | return expression_opt ';'	{% withNodeInfo $1 $ CReturn $2 }


-- parse GNU C __asm__ statement (compatible with C99: J.5.10)
--
-- asm_stmt    :- asm volatile? ( "asm..." : output-operands : input-operands : asm-clobbers )
-- asm_operand :- [operand-name] "constraint" ( expr )
-- asm_clobber :- "r1", "r2", ...
--
asm_statement :: { CAsmStmt }
asm_statement
  : asm maybe_type_qualifier '(' string_literal ')' ';'
  	{% withNodeInfo $1 $ CAsmStmt $2 $4 [] [] [] }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ')' ';'
  	{% withNodeInfo $1 $ CAsmStmt $2 $4 $6 [] [] }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ':' asm_operands ')' ';'
  	{% withNodeInfo $1 $ CAsmStmt $2 $4 $6 $8 [] }

  | asm maybe_type_qualifier '(' string_literal ':' asm_operands ':' asm_operands ':' asm_clobbers ')' ';'
  	{% withNodeInfo $1 $ CAsmStmt $2 $4 $6 $8 (reverse $10) }


maybe_type_qualifier :: { Maybe CTypeQual }
maybe_type_qualifier
  : {- empty -}		  { Nothing }
  | type_qualifier	  { Just $1 }

asm_operands :: { [CAsmOperand] }
asm_operands
  : {- empty -}				{ [] }
  | nonnull_asm_operands    { reverse $1 }

nonnull_asm_operands :: { Reversed [CAsmOperand] }
nonnull_asm_operands
  : asm_operand					          { singleton $1 }
  | nonnull_asm_operands ',' asm_operand  { $1 `snoc` $3 }

asm_operand :: { CAsmOperand }
asm_operand
  : string_literal '(' expression ')'			      {% withNodeInfo $1 $ CAsmOperand Nothing $1 $3 }
  | '[' ident ']' string_literal '(' expression ')'   {% withNodeInfo $1 $ CAsmOperand (Just $2) $4 $6 }
  | '[' tyident ']' string_literal '(' expression ')' {% withNodeInfo $1 $ CAsmOperand (Just $2) $4 $6 }


asm_clobbers :: { Reversed [CStrLit] }
asm_clobbers
  : string_literal			        { singleton $1 }
  | asm_clobbers ',' string_literal	{ $1 `snoc` $3 }

{-
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
-- Declarations
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

Declarations are the most complicated part of the grammar, and shall be summarized here.
To allow a lightweight notation, we will use the modifier <permute> to indicate that the order of the immidieate right-hand
sides doesn't matter.
 - <permute> a* b+ c   === any sequence of a's, b's and c's, which contains exactly 1 'c' and at least one 'b'

-- storage class and type qualifier
---------------------------------------------------------------------------------------------------------------
attr                       :-   __attribute__((..))
storage_class              :-   typedef | extern | static | auto | register | __thread
type_qualifier             :-   const | volatile | restrict | inline
type_qualifier_list        :-   type_qualifier+

declaration_qualifier      :-   storage_class | type_qualifier
declaration_qualifier_list :-   <permute> type_qualifier* storage_class+

qualifiers                 :-   declaration_qualifier_list | type_qualifier_list
                           :=   <permute> (type_qualifier|storage_class)+

-- type names
---------------------------------------------------------------------------------------------------------------
declaration_specifier      :- <permute> type_qualifier* storage_class+ (basic_type_name+ | elaborated_type_name | tyident )
type_specifier             :- <permute> type_qualifier* (basic_type_name+ | elaborated_type_name | tyident)

specifiers                 :- declaration_specifier | type_specifier
                           := <permute> type_qualifier* storage_class* (basic_type_name+ | elaborated_type_name | tyident )

-- struct/union/enum declarations
---------------------------------------------------------------------------------------------------------------
sue_declaration_specifier :- <permute> type_qualifier* storage_class+ elaborated_type_name
sue_type_specifier        :- <permute> type_qualifier* elaborated_type_name

sue_declaration           := sue_declaration_specifier | sue_type_specifier
                          :- <permute> type_qualifier* storage_class* elaborated_type_name

-- declarators
---------------------------------------------------------------------------------------------------------------
identifier_declarator :- ( '*' (type_qualifier | attr)* ) * ident     [ array_decl | "(" parameter-list ")" ]
                               plus additional parenthesis' ending ^^ here
typedef_declartor     :-
declarator            :- identifier_declarator | typedef_declarator

-- Declaration lists
---------------------------------------------------------------------------------------------------------------
default_declaring_list :- qualifiers ( identifier_declarator asm*attrs* initializer? )_comma_list

declaring_list         :- specifiers ( declarator asm*attrs* initializer? )_comma_list

declaration_list := default_declaring_list | declaring_list

-- Declaration
---------------------------------------------------------------------------------------------------------------
declaration = sue_declaration | declaration_list

---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
-- Attributes
-- (citing http://gcc.gnu.org/onlinedocs/gcc/Attribute-Syntax.html)
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

"Attributes may appear after the colon following a label (expect case and default)"

labeled_statement :- identifier ':' attrs_opt statement

"Attributes may go either immediately after the struct/union/enum keyword or after the closing brace"

struct attrs_opt ...
struct ... { } attrs_opt

"In general: Attributes appear as part of declarations, either belonging to a declaration or declarator"

"Any list of specifiers and qualifiers at the start of a declaration may contain attribute specifiers"
"An attribute list may appear immediately before the comma, = or semicolon terminating a declaration of an identifier"

---------------------------------------------------------------------------------------------------------------
For the parser, we modified the following rules to be interleaved with attributes:

default_declaring_list' :-  (declaration_qualifier_list' | type_qualifier_list' attr*)
                                             identifier_declarator asm*attr* initializer?
                                 { ',' attr* identifier_declarator asm*attr* initializer? }
declaring_list' :-          specifier' declarator asm*attr* initializer?
                                 { ',' attr* declarator asm*attr* initializer? }


type_qualifier_list' is like type_qualifier_list, but with preceeding and/or interleaving (but not terminating) __attribute__ annotations.
declaration_qualifier_list', declaration_specifier' and type_specifier' are like their unprimed variants, but with arbitrary preceeding, interleaving and/or terminating __attribute__ annotations.

"An attribute list may appear immediately before a declarator other than the first in a comma seperated list of declarators"

"The attribute specifiers may be the only specifiers present (implicit int)" [not supported]

"Attribute specifiers may be mixed with type qualifiers appearing inside the [] of an parameter array declarator"

tbc.
-}



-- parse C declaration (C99 6.7)
declaration :: { CDecl }
declaration
  : sue_declaration_specifier ';'
  	{% withNodeInfo $1 $ CDecl (reverse $1) [] }

  | sue_type_specifier ';'
  	{% withNodeInfo $1 $ CDecl (reverse $1) [] }

  | declaring_list ';'
  	{% case $1 of CDecl declspecs dies at -> withLength at (CDecl declspecs (List.reverse dies)) }

  | default_declaring_list ';'
  	{% case $1 of CDecl declspecs dies at -> withLength at (CDecl declspecs (List.reverse dies)) }


declaration_list :: { Reversed [CDecl] }
declaration_list
  : {- empty -}					 { empty }
  | declaration_list declaration { $1 `snoc` $2 }


-- * SUMMARY: default_declaring_list :- qualifier* identifier_declarator asm_attrs initializer?
--                                                 { ',' identifier_declarator asm_attrs initializer? }
--
-- * GNU extensions
--   __attribute__ annotations imm. before an declarator (see Attribute Syntax, paragraph 11)
--   asm + __attribute__ annotations (end of declarations, see Attribute Syntax, paragraph 12)
--   The assembler annotation is used to specifiy an assembler name for the declarator.
--
default_declaring_list :: { CDecl }
default_declaring_list
  : declaration_qualifier_list identifier_declarator asm_attrs_opt {-{}-} initializer_opt
  	{% let declspecs = reverse $1 in
  	   do{ declr <- withAsmNameAttrs $3 $2
           ; doDeclIdent declspecs declr
           ; withNodeInfo $1 $
                CDecl declspecs [(Just (reverseDeclr declr), $4, Nothing)] }}

  | type_qualifier_list identifier_declarator asm_attrs_opt {-{}-} initializer_opt
  	{% let declspecs = liftTypeQuals $1 in
  	   do{ declr <- withAsmNameAttrs $3 $2
           ; doDeclIdent declspecs declr
           ; withNodeInfo $1 $ CDecl declspecs [(Just (reverseDeclr declr), $4, Nothing)] }}

  | type_qualifier_list attrs identifier_declarator asm_attrs_opt {-{}-} initializer_opt -- FIX 1600
  	{% let declspecs = liftTypeQuals $1 in
  	   do{ declr <- withAsmNameAttrs $4 $3
           ; doDeclIdent declspecs declr
           ; withNodeInfo $1 $ CDecl (declspecs ++ liftCAttrs $2) [(Just (reverseDeclr declr), $5, Nothing)] }}

  -- GNU extension: __attribute__ as the only qualifier
  | attrs identifier_declarator asm_attrs_opt {-{}-} initializer_opt
    {% let declspecs = liftCAttrs $1 in
       do{ declr <- withAsmNameAttrs $3 $2
           ; doDeclIdent declspecs declr
           ; withNodeInfo $1 $ CDecl declspecs [(Just (reverseDeclr declr), $4, Nothing)] }}

  | default_declaring_list ',' attrs_opt identifier_declarator asm_attrs_opt {-{}-} initializer_opt
  	{% case $1 of
             CDecl declspecs dies at -> do
               declr <- withAsmNameAttrs (fst $5, snd $5 ++ $3) $4
               doDeclIdent declspecs declr
               withLength at $ CDecl declspecs ((Just (reverseDeclr declr), $6, Nothing) : dies)  }

-- assembler, followed by attribute annotation
asm_attrs_opt :: { (Maybe CStrLit, [CAttr]) }
asm_attrs_opt
  : asm_opt attrs_opt
  { ($1,$2) }

--
-- SUMMARY: declaring_list :- specifier* declarator asm_attrs initializer?
--                                 { ',' declarator asm_attrs initializer? }
--
-- GNU extensions:
--      __attribute__ annotations imm. before an declarator (see Attribute Syntax, paragraph 11)
--      asm + __attribute__ annotations (end of declarations, see Attribute Syntax, paragraph 12)
--
declaring_list :: { CDecl }
declaring_list
  : declaration_specifier declarator asm_attrs_opt initializer_opt
  	{% do{
  	   declr <- withAsmNameAttrs $3 $2;
  	   doDeclIdent $1 declr;
       withNodeInfo $1 $ CDecl $1 [(Just (reverseDeclr declr), $4, Nothing)] }
    }

  | type_specifier declarator asm_attrs_opt initializer_opt
  	{% do{
  	   declr <- withAsmNameAttrs $3 $2;
  	   doDeclIdent $1 declr;
       withNodeInfo $1 $ CDecl $1 [(Just (reverseDeclr declr), $4, Nothing)] }
    }

  | declaring_list ',' attrs_opt declarator asm_attrs_opt initializer_opt
  	{% case $1 of
             CDecl declspecs dies at -> do
               declr <- withAsmNameAttrs (fst $5, snd $5 ++ $3) $4
               doDeclIdent declspecs declr
               return (CDecl declspecs ((Just (reverseDeclr declr), $6, Nothing) : dies) at) }


-- parse C declaration specifiers (C99 6.7)
--
-- * <permute> type_qualifier* storage_class+ (basic_type_name+ | elaborated_type_name | tyident )
--
declaration_specifier :: { [CDeclSpec] }
declaration_specifier
  : basic_declaration_specifier		{ reverse $1 }	-- Arithmetic or void
  | sue_declaration_specifier		{ reverse $1 }	  -- Struct/Union/Enum
  | typedef_declaration_specifier	{ reverse $1 }	-- Typedef


-- A mixture of type qualifiers (const, volatile, restrict, inline) and storage class specifiers
-- (extern, static, auto, register, __thread), in any order, but containing at least one storage class specifier.
--
-- declaration_qualifier_list :- <permute> type_qualifier* storage_class+
--
-- GNU extensions
--   * arbitrary interleaved __attribute__ annotations
--
declaration_qualifier_list :: { Reversed [CDeclSpec] }
declaration_qualifier_list
  : storage_class
  	{ singleton (CStorageSpec $1) }

  | attrs storage_class
  	{ reverseList (liftCAttrs $1) `snoc` (CStorageSpec $2) }

  | type_qualifier_list storage_class
  	{ rmap CTypeQual $1 `snoc` CStorageSpec $2 }

  | type_qualifier_list attrs storage_class
  	{ (rmap CTypeQual $1 `rappend` liftCAttrs $2) `snoc` CStorageSpec $3 }

  | declaration_qualifier_list declaration_qualifier
  	{ $1 `snoc` $2 }

  | declaration_qualifier_list attr
  	{ addTrailingAttrs $1 $2 }

--
-- declaration_qualifier :- storage_class | type_qualifier
--
declaration_qualifier :: { CDeclSpec }
declaration_qualifier
  : storage_class		{ CStorageSpec $1 }
  | type_qualifier		{ CTypeQual $1 }     -- const or volatile


-- parse C storage class specifier (C99 6.7.1)
--
-- * GNU extensions: '__thread' thread local storage
--
storage_class :: { CStorageSpec }
storage_class
  : typedef			{% withNodeInfo $1 $ CTypedef }
  | extern			{% withNodeInfo $1 $ CExtern }
  | static			{% withNodeInfo $1 $ CStatic }
  | auto			{% withNodeInfo $1 $ CAuto }
  | register			{% withNodeInfo $1 $ CRegister }
  | "__thread"			{% withNodeInfo $1 $ CThread }


-- parse C type specifier (C99 6.7.2)
--
-- This recignises a whole list of type specifiers rather than just one
-- as in the C99 grammar.
--
-- type_specifier :- <permute> type_qualifier* (basic_type_name+ | elaborated_type_name | g)
--
type_specifier :: { [CDeclSpec] }
type_specifier
  : basic_type_specifier		{ reverse $1 }	-- Arithmetic or void
  | sue_type_specifier			{ reverse $1 }	-- Struct/Union/Enum
  | typedef_type_specifier		{ reverse $1 }	-- Typedef

basic_type_name :: { CTypeSpec }
basic_type_name
  : void			{% withNodeInfo $1 $ CVoidType }
  | char			{% withNodeInfo $1 $ CCharType }
  | short			{% withNodeInfo $1 $ CShortType }
  | int				{% withNodeInfo $1 $ CIntType }
  | long			{% withNodeInfo $1 $ CLongType }
  | float			{% withNodeInfo $1 $ CFloatType }
  | double			{% withNodeInfo $1 $ CDoubleType }
  | signed			{% withNodeInfo $1 $ CSignedType }
  | unsigned			{% withNodeInfo $1 $ CUnsigType }
  | "_Bool"			{% withNodeInfo $1 $ CBoolType }
  | "_Complex"			{% withNodeInfo $1 $ CComplexType }
  | "__int128"      {% withNodeInfo $1 $ CInt128Type }
  | "__float128"      {% withNodeInfo $1 $ CFloat128Type }
  | "_Float128"      {% withNodeInfo $1 $ CFloat128Type }


-- A mixture of type qualifiers, storage class and basic type names in any
-- order, but containing at least one basic type name and at least one storage
-- class specifier.
--
-- basic_declaration_specifier :- <permute> type_qualifier* storage_class+ basic_type_name+
--
--   GNU extensions
--     arbitrary interleaved __attribute__ annotations
--
basic_declaration_specifier :: { Reversed [CDeclSpec] }
basic_declaration_specifier
  : declaration_qualifier_list basic_type_name
  	{ $1 `snoc` CTypeSpec $2 }

  | basic_type_specifier storage_class
  	{ $1 `snoc` CStorageSpec $2 }

  | basic_declaration_specifier declaration_qualifier
  	{ $1 `snoc` $2 }

  | basic_declaration_specifier basic_type_name
  	{ $1 `snoc` CTypeSpec $2 }

  | basic_declaration_specifier attr
  	{ addTrailingAttrs $1 $2 }


-- A mixture of type qualifiers and basic type names in any order, but
-- containing at least one basic type name.
--
-- basic_type_specifier :- <permute> type_qualifier* basic_type_name+
--
--   GNU extensions
--     arbitrary interleaved __attribute__ annotations
--
basic_type_specifier :: { Reversed [CDeclSpec] }
basic_type_specifier
  -- Arithmetic or void
  : basic_type_name
  	{ singleton (CTypeSpec $1) }

  | attrs basic_type_name
  	{ (reverseList $ liftCAttrs $1) `snoc` (CTypeSpec $2) }

  | type_qualifier_list basic_type_name
  	{ rmap CTypeQual $1 `snoc` CTypeSpec $2 }

  | type_qualifier_list attrs basic_type_name
  	{ rmap CTypeQual $1 `rappend` (liftCAttrs $2) `snoc` CTypeSpec $3 }

  | basic_type_specifier type_qualifier
  	{ $1 `snoc` CTypeQual $2 }

  | basic_type_specifier basic_type_name
  	{ $1 `snoc` CTypeSpec $2 }

  | basic_type_specifier attr
     { addTrailingAttrs $1 $2 }


-- A named or anonymous struct, union or enum type along with at least one
-- storage class and any mix of type qualifiers.
--
-- * Summary:
--   sue_declaration_specifier :- <permute> type_qualifier* storage_class+ elaborated_type_name
--
sue_declaration_specifier :: { Reversed [CDeclSpec] }
sue_declaration_specifier
  : declaration_qualifier_list elaborated_type_name
  	{ $1 `snoc` CTypeSpec $2 }

  | sue_type_specifier storage_class
  	{ $1 `snoc` CStorageSpec $2 }

  | sue_declaration_specifier declaration_qualifier
  	{ $1 `snoc` $2 }

  | sue_declaration_specifier attr
  	{ addTrailingAttrs $1 $2 }


-- A struct, union or enum type (named or anonymous) with optional leading and
-- trailing type qualifiers.
--
-- * Summary:
--   sue_type_specifier :- <permute> type_qualifier* elaborated_type_name
--
-- * GNU Extensions: records __attribute__ annotations
--
sue_type_specifier :: { Reversed [CDeclSpec] }
sue_type_specifier
  -- struct/union/enum
  : elaborated_type_name
  	{ singleton (CTypeSpec $1) }

  | attrs elaborated_type_name
  	{ (reverseList $ liftCAttrs $1) `snoc` (CTypeSpec $2) }

  | type_qualifier_list elaborated_type_name
  	{ rmap CTypeQual $1 `snoc` CTypeSpec $2 }

  | type_qualifier_list attrs elaborated_type_name
  	{ rmap CTypeQual  $1 `rappend` (liftCAttrs $2) `snoc` CTypeSpec $3 }

  | sue_type_specifier type_qualifier
  	{ $1 `snoc` CTypeQual $2 }

  | sue_type_specifier attr
    { addTrailingAttrs $1 $2 }

-- A typedef'ed type identifier with at least one storage qualifier and any
-- number of type qualifiers
--
-- * Summary:
--   typedef_declaration_specifier :- <permute> type_qualifier* storage_class+ tyident
--
-- * Note:
--   the tyident can also be a: typeof '(' ... ')'
--
typedef_declaration_specifier :: { Reversed [CDeclSpec] }
typedef_declaration_specifier
  : typedef_type_specifier storage_class
  	{ $1 `snoc` CStorageSpec $2 }

  | declaration_qualifier_list tyident
  	{% withNodeInfo $2 $ \at -> $1 `snoc` CTypeSpec (CTypeDef $2 at) }

  | declaration_qualifier_list typeof '(' expression ')'
  	{% withNodeInfo $2 $ \at -> $1 `snoc` CTypeSpec (CTypeOfExpr $4 at) }

  | declaration_qualifier_list typeof '(' type_name ')'
  	{% withNodeInfo $2 $ \at -> $1 `snoc` CTypeSpec (CTypeOfType $4 at) }

  | typedef_declaration_specifier declaration_qualifier
  	{ $1 `snoc` $2 }

  | typedef_declaration_specifier attr
  	{ addTrailingAttrs $1 $2 }


-- typedef'ed type identifier with optional leading and trailing type qualifiers
--
-- * Summary:
--   type_qualifier* ( tyident | typeof '('...')' ) type_qualifier*
--
typedef_type_specifier :: { Reversed [CDeclSpec] }
typedef_type_specifier
  : tyident
  	{% withNodeInfo $1 $ \at -> singleton (CTypeSpec (CTypeDef $1 at)) }

  | typeof '(' expression ')'
  	{% withNodeInfo $1 $ \at -> singleton (CTypeSpec (CTypeOfExpr $3 at)) }

  | typeof '(' type_name ')'
  	{% withNodeInfo $1 $ \at -> singleton (CTypeSpec (CTypeOfType $3 at)) }

  | type_qualifier_list tyident
  	{% withNodeInfo $2 $ \at -> rmap CTypeQual  $1 `snoc` CTypeSpec (CTypeDef $2 at) }

  | type_qualifier_list typeof '(' expression ')'
  	{% withNodeInfo $2 $ \at -> rmap CTypeQual  $1 `snoc` CTypeSpec (CTypeOfExpr $4 at) }

  | type_qualifier_list typeof '(' type_name ')'
  	{% withNodeInfo $2 $ \at -> rmap CTypeQual  $1 `snoc` CTypeSpec (CTypeOfType $4 at) }

  -- repeat with attrs (this could be easier if type qualifier list wouldn't allow leading attributes)
  | attrs tyident
  	{% withNodeInfo $2 $ \at -> reverseList (liftCAttrs $1) `snoc` (CTypeSpec (CTypeDef $2 at)) }

  | attrs typeof '(' expression ')'
  	{% withNodeInfo $1 $ \at -> reverseList (liftCAttrs $1) `snoc`  (CTypeSpec (CTypeOfExpr $4 at)) }

  | attrs typeof '(' type_name ')'
  	{% withNodeInfo $2 $ \at -> reverseList (liftCAttrs $1) `snoc`  (CTypeSpec (CTypeOfType $4 at)) }

  | type_qualifier_list attrs tyident
  	{% withNodeInfo $3 $ \at -> rmap CTypeQual  $1 `rappend` (liftCAttrs $2) `snoc` CTypeSpec (CTypeDef $3 at) }

  | type_qualifier_list attrs typeof '(' expression ')'
  	{% withNodeInfo $3 $ \at -> rmap CTypeQual  $1 `rappend` (liftCAttrs $2) `snoc` CTypeSpec (CTypeOfExpr $5 at) }

  | type_qualifier_list attrs typeof '(' type_name ')'
  	{% withNodeInfo $3 $ \at -> rmap CTypeQual  $1 `rappend` (liftCAttrs $2) `snoc` CTypeSpec (CTypeOfType $5 at) }

  | typedef_type_specifier type_qualifier
  	{ $1 `snoc` CTypeQual $2 }

  | typedef_type_specifier attr
  	{ addTrailingAttrs $1 $2 }


-- A named or anonymous struct, union or enum type.
--
-- * Summary:
--   (struct|union|enum) (identifier? '{' ... '}' | identifier)
--
elaborated_type_name :: { CTypeSpec }
elaborated_type_name
  : struct_or_union_specifier	{% withNodeInfo $1 $ CSUType $1 }
  | enum_specifier		{% withNodeInfo $1 $ CEnumType $1 }


-- parse C structure or union declaration (C99 6.7.2.1)
--
-- * Summary:
--    (struct|union) (identifier? '{' ... '}' | identifier)
--
struct_or_union_specifier :: { CStructUnion }
struct_or_union_specifier
  : struct_or_union attrs_opt identifier '{' struct_declaration_list  '}'
  	{% withNodeInfo $1 $ CStruct (unL $1) (Just $3) (Just$ reverse $5) $2 }

  | struct_or_union attrs_opt '{' struct_declaration_list  '}'
  	{% withNodeInfo $1 $ CStruct (unL $1) Nothing   (Just$ reverse $4) $2 }

  | struct_or_union attrs_opt identifier
  	{% withNodeInfo $1 $ CStruct (unL $1) (Just $3) Nothing $2 }


struct_or_union :: { Located CStructTag }
struct_or_union
  : struct			{ L CStructTag (posOf $1) }
  | union			{ L CUnionTag (posOf $1) }


struct_declaration_list :: { Reversed [CDecl] }
struct_declaration_list
  : {- empty -}						{ empty }
  | struct_declaration_list ';'				{ $1 }
  | struct_declaration_list struct_declaration		{ $1 `snoc` $2 }


-- parse C structure declaration (C99 6.7.2.1)
--
struct_declaration :: { CDecl }
struct_declaration
  : struct_declaring_list ';'
  	{ case $1 of CDecl declspecs dies at -> CDecl declspecs (List.reverse dies) at }

  | struct_default_declaring_list';'
  	{ case $1 of CDecl declspecs dies at -> CDecl declspecs (List.reverse dies) at }

  | "__extension__" struct_declaration	{ $2 }


--
--  * Note: doesn't redeclare typedef
--
--  TODO: FIXME: AST doesn't allow recording attributes of unnamed struct members
struct_default_declaring_list :: { CDecl }
struct_default_declaring_list
  : type_qualifier_list attrs_opt struct_identifier_declarator
  	{% withNodeInfo $1 $ case $3 of (d,s) -> CDecl (liftTypeQuals $1 ++ liftCAttrs $2) [(d,Nothing,s)] }

  -- GNU extension: __attribute__ as only type qualifier
  | attrs struct_identifier_declarator
    {% withNodeInfo $1 $ case $2 of (d,s) -> CDecl (liftCAttrs $1) [(d,Nothing,s)] }
  -- attrs_opt apply to the declared object
  | struct_default_declaring_list ',' attrs_opt struct_identifier_declarator
  	{ case $1 of
            CDecl declspecs dies at ->
              case $4 of
                (Just d,s) -> CDecl declspecs ((Just $ appendObjAttrs $3 d,Nothing,s) : dies) at
                (Nothing,s) -> CDecl declspecs ((Nothing,Nothing,s) : dies) at } -- FIXME

-- * GNU extensions:
--     allow anonymous nested structures and unions
--     FIXME: cannot record attribute of unnamed field
struct_declaring_list :: { CDecl }
struct_declaring_list
  : type_specifier struct_declarator attrs_opt
  	{% withNodeInfo $1 $ case $2 of { (Just d,s)  -> CDecl $1 [(Just $! appendObjAttrs $3 d,Nothing,s)]
                                    ; (Nothing,s) -> CDecl $1 [(Nothing,Nothing,s)]  } } {- DO FIXME -}
  | struct_declaring_list ',' attrs_opt struct_declarator attrs_opt
  	{ case $1 of
            CDecl declspecs dies attr ->
              case $4 of
                (Just d,s) -> CDecl declspecs ((Just$ appendObjAttrs ($3++$5) d,Nothing,s) : dies) attr
                (Nothing,s) -> CDecl declspecs ((Nothing,Nothing,s) : dies) attr }

  -- FIXME: We're being far too liberal in the parsing here, we really want to just
  -- allow unnamed struct and union fields but we're actually allowing any
  -- unnamed struct member. Making it allow only unnamed structs or unions in
  -- the parser is far too tricky, it makes things ambiguous. So we'll have to
  -- diagnose unnamed fields that are not structs/unions in a later stage.

  -- Note that a plain type specifier can have a trailing attribute

  | type_specifier
     {% withNodeInfo $1 $ CDecl $1 []  }


-- parse C structure declarator (C99 6.7.2.1)
--
struct_declarator :: { (Maybe CDeclr, Maybe CExpr) }
struct_declarator
  : declarator					       { (Just (reverseDeclr $1), Nothing) }
  | ':' constant_expression			   { (Nothing, Just $2) }
  | declarator ':' constant_expression { (Just (reverseDeclr $1), Just $3) }

-- FIXME: anonymous bitfield doesn't allow recording of attributes
struct_identifier_declarator :: { (Maybe CDeclr, Maybe CExpr) }
struct_identifier_declarator
  : identifier_declarator				{ (Just (reverseDeclr $1), Nothing) }
  | ':' constant_expression				{ (Nothing, Just $2) }
  | identifier_declarator ':' constant_expression	{ (Just (reverseDeclr $1), Just $3) }
  | struct_identifier_declarator attr
    {  case $1 of {   (Nothing,expr) -> (Nothing,expr) {- FIXME -}
                    ; (Just (CDeclr name derived asmname attrs node), bsz) ->
                                        (Just (CDeclr name derived asmname (attrs++$2) node),bsz)
                  }
    }

-- parse C enumeration declaration (C99 6.7.2.2)
--
-- * Summary:
--   enum (identifier? '{' ... '}' | identifier)
--
enum_specifier :: { CEnum }
enum_specifier
  : enum attrs_opt '{' enumerator_list '}'
  	{% withNodeInfo $1 $ CEnum Nothing   (Just$ reverse $4) $2 }

  | enum attrs_opt '{' enumerator_list ',' '}'
  	{% withNodeInfo $1 $ CEnum Nothing   (Just$ reverse $4) $2 }

  | enum attrs_opt identifier '{' enumerator_list '}'
  	{% withNodeInfo $1 $ CEnum (Just $3) (Just$ reverse $5) $2 }

  | enum attrs_opt identifier '{' enumerator_list ',' '}'
  	{% withNodeInfo $1 $ CEnum (Just $3) (Just$ reverse $5) $2 }

  | enum attrs_opt identifier
  	{% withNodeInfo $1 $ CEnum (Just $3) Nothing $2           }

enumerator_list :: { Reversed [(Ident, Maybe CExpr)] }
enumerator_list
  : enumerator					{ singleton $1 }
  | enumerator_list ',' enumerator		{ $1 `snoc` $3 }


enumerator :: { (Ident, Maybe CExpr) }
enumerator
  : identifier                              { ($1, Nothing) }
  | identifier attr                         { ($1, Nothing) }
  | identifier attr '=' constant_expression { ($1, Just $4) }
  | identifier '=' constant_expression      { ($1, Just $3) }


-- parse C type qualifier (C99 6.7.3)
--
type_qualifier :: { CTypeQual }
type_qualifier
  : const		{% withNodeInfo $1 $ CConstQual }
  | volatile		{% withNodeInfo $1 $ CVolatQual }
  | restrict		{% withNodeInfo $1 $ CRestrQual }
  | inline		{% withNodeInfo $1 $ CInlineQual }

-- a list containing at least one type_qualifier (const, volatile, restrict, inline)
--    and additionally CAttrs
type_qualifier_list :: { Reversed [CTypeQual] }
type_qualifier_list
  : attrs_opt type_qualifier	             { reverseList (map CAttrQual $1) `snoc` $2 }
  | type_qualifier_list type_qualifier	     { $1 `snoc` $2 }
  | type_qualifier_list attrs type_qualifier { ($1 `rappend` map CAttrQual $2) `snoc` $3}

-- parse C declarator (C99 6.7.5)
--
declarator :: { CDeclrR }
declarator
  : identifier_declarator		{ $1 }
  | typedef_declarator			{ $1 }


-- Parse GNU C's asm annotations
--
-- Those annotations allow to give an assembler name to a function or identifier.
asm_opt :: { Maybe CStrLit }
asm_opt
  : {- empty -}				          { Nothing }
  | asm '(' string_literal ')'	{ Just $3 }

--
-- typedef_declarator :-

typedef_declarator :: { CDeclrR }
typedef_declarator
  -- would be ambiguous as parameter
  : paren_typedef_declarator		{ $1 }

  -- not ambiguous as param
  | parameter_typedef_declarator	{ $1 }


-- parameter_typedef_declarator :- tyident declarator_postfix?
--                              | '(' attrs? clean_typedef_declarator ')' declarator_postfix?
--                              |  '*' attrs? type_qualifier_list? parameter_typedef_declarator
--
parameter_typedef_declarator :: { CDeclrR }
parameter_typedef_declarator
  : tyident
  	{% withNodeInfo $1 $ mkVarDeclr $1 }

  | tyident postfixing_abstract_declarator
  	{% withNodeInfo $1 $ \at -> $2 (mkVarDeclr $1 at) }

  | clean_typedef_declarator
  	{ $1 }


-- The  following have at least one '*'.
-- There is no (redundant) '(' between the '*' and the tyident.
--
-- clean_typedef_declarator :-  '(' attrs? clean_typedef_declarator ')' declarator_postfix?
--                            | '*' attrs? type_qualifier_list? parameter_typedef_declarator
--
clean_typedef_declarator :: { CDeclrR }
clean_typedef_declarator
  : clean_postfix_typedef_declarator
  	{ $1 }

  | '*' parameter_typedef_declarator
  	{% withNodeInfo $1 $ ptrDeclr $2 [] }

  | '*' attrs parameter_typedef_declarator
  	{% withAttribute $1 $2 $ ptrDeclr $3 [] }

  | '*' type_qualifier_list  parameter_typedef_declarator
  	{% withNodeInfo $1 $ ptrDeclr $3 (reverse $2) }

  | '*' type_qualifier_list attrs parameter_typedef_declarator
  	{% withAttribute $1 $3 $ ptrDeclr $4 (reverse $2)  }

-- clean_postfix_typedef_declarator :- ( attrs? clean_typedef_declarator ) declarator_postfix?
--
clean_postfix_typedef_declarator :: { CDeclrR }
clean_postfix_typedef_declarator
  : '(' clean_typedef_declarator ')'						              { $2 }
  | '(' clean_typedef_declarator ')' postfixing_abstract_declarator		  { $4 $2 }
  | '(' attrs clean_typedef_declarator ')'	                              { appendDeclrAttrs $2 $3 }
  | '(' attrs clean_typedef_declarator ')' postfixing_abstract_declarator { appendDeclrAttrs $2 ($5 $3) }


-- The following have a redundant '(' placed
-- immediately to the left of the tyident
--
paren_typedef_declarator :: { CDeclrR }
paren_typedef_declarator
  : paren_postfix_typedef_declarator
  	{ $1 }

  -- redundant paren
  | '*' '(' simple_paren_typedef_declarator ')'
  	{% withNodeInfo $1 $ ptrDeclr $3 [] }

  | '*' type_qualifier_list '(' simple_paren_typedef_declarator ')'
  	{% withNodeInfo $1 $ ptrDeclr $4 (reverse $2) }
  | '*' type_qualifier_list attrs '(' simple_paren_typedef_declarator ')'
  	{% withAttribute $1 $3 $ ptrDeclr $5 (reverse $2)  }

  | '*' paren_typedef_declarator
  	{% withNodeInfo $1 $ ptrDeclr $2 [] }

  | '*' type_qualifier_list paren_typedef_declarator
  	{% withNodeInfo $1 $ ptrDeclr $3 (reverse $2) }
  | '*' type_qualifier_list attrs paren_typedef_declarator
  	{% withAttribute $1 $3 $ ptrDeclr $4 (reverse $2) }

-- redundant paren to left of tname
paren_postfix_typedef_declarator :: { CDeclrR }
paren_postfix_typedef_declarator
  : '(' paren_typedef_declarator ')'
  	{ $2 }

  -- redundant paren
  | '(' simple_paren_typedef_declarator postfixing_abstract_declarator ')'
  	{ $3 $2 }

  | '(' paren_typedef_declarator ')' postfixing_abstract_declarator
  	{ $4 $2 }


-- Just a type name in any number of nested brackets
--
simple_paren_typedef_declarator :: { CDeclrR }
simple_paren_typedef_declarator
  : tyident
  	{% withNodeInfo $1 $ mkVarDeclr $1 }

  | '(' simple_paren_typedef_declarator ')'
  	{ $2 }

--
-- Declarators
-- * Summary
--   declarator :- ( '*' (type_qualifier | attr)* )* ident ( array_decl | "(" parameter-list ")" )?
--      + additional parenthesis
--
identifier_declarator :: { CDeclrR }
identifier_declarator
  : unary_identifier_declarator			{ $1 }
  | paren_identifier_declarator			{ $1 }


unary_identifier_declarator :: { CDeclrR }
unary_identifier_declarator
  : postfix_identifier_declarator
  	{ $1 }

  | '*' identifier_declarator
  	{% withNodeInfo $1 $ ptrDeclr $2 [] }

  | '*' attrs identifier_declarator
  	{% withAttribute $1 $2 $ ptrDeclr $3 [] }

  | '*' type_qualifier_list identifier_declarator
  	{% withNodeInfo $1 $ ptrDeclr $3 (reverse $2) }

  | '*' type_qualifier_list attrs identifier_declarator
  	{% withAttribute $1 $3 $ ptrDeclr $4 (reverse $2) }

postfix_identifier_declarator :: { CDeclrR }
postfix_identifier_declarator
  : paren_identifier_declarator postfixing_abstract_declarator
  	{ $2 $1 }

   | '('  unary_identifier_declarator ')'
   	{ $2 }

   | '(' unary_identifier_declarator ')' postfixing_abstract_declarator
   	{ $4 $2 }

   | '(' attrs unary_identifier_declarator ')'
     { appendDeclrAttrs $2 $3 }

   | '(' attrs unary_identifier_declarator ')' postfixing_abstract_declarator
     { appendDeclrAttrs $2 ($5 $3) }


-- just an identifier in any number of nested parenthesis
paren_identifier_declarator :: { CDeclrR }
paren_identifier_declarator
  : ident
  	{% withNodeInfo $1 $ mkVarDeclr $1 }

  | '(' paren_identifier_declarator ')'
  	{ $2 }

  | '(' attrs paren_identifier_declarator ')'
  	{ appendDeclrAttrs $2 $3 }

function_declarator_old :: { CDeclr }
function_declarator_old
  : old_function_declarator
    { reverseDeclr $1 }

old_function_declarator :: { CDeclrR }
old_function_declarator
  : postfix_old_function_declarator
  	{ $1 }

  | '*' old_function_declarator
  	{% withNodeInfo $1 $ ptrDeclr $2 [] } -- FIXME: no attr possible here ???

  | '*' type_qualifier_list old_function_declarator
  	{% withNodeInfo $1 $ ptrDeclr $3 (reverse $2) }

postfix_old_function_declarator :: { CDeclrR }
postfix_old_function_declarator
  : paren_identifier_declarator '(' identifier_list ')'
  	{% withNodeInfo $1 $ funDeclr $1 (Left $ reverse $3) [] }

  | '(' old_function_declarator ')'
  	{ $2 }

  | '(' old_function_declarator ')' postfixing_abstract_declarator
  	{ $4 $2 }


-- parse C parameter type list (C99 6.7.5)
--
parameter_type_list :: { ([CDecl], Bool) }
parameter_type_list
  : {- empty -}				{ ([], False)}
  | parameter_list			{ (reverse $1, False) }
  | parameter_list ',' "..."		{ (reverse $1, True) }

parameter_list :: { Reversed [CDecl] }
parameter_list
  : parameter_declaration				{ singleton $1 }
  | parameter_list ',' parameter_declaration	{ $1 `snoc` $3 }

parameter_declaration :: { CDecl }
parameter_declaration
  : declaration_specifier
  	{% withNodeInfo $1 $ CDecl $1 [] }

  | declaration_specifier abstract_declarator
  	{% withNodeInfo $1 $ CDecl $1 [(Just (reverseDeclr $2), Nothing, Nothing)] }

  | declaration_specifier identifier_declarator attrs_opt
  	{% withNodeInfo $1 $ CDecl $1 [(Just (reverseDeclr $! appendDeclrAttrs $3 $2), Nothing, Nothing)] }

  | declaration_specifier parameter_typedef_declarator attrs_opt
  	{% withNodeInfo $1 $ CDecl $1 [(Just (reverseDeclr $! appendDeclrAttrs $3 $2), Nothing, Nothing)] }

  | declaration_qualifier_list
  	{% withNodeInfo $1 $ CDecl (reverse $1) [] }

  | declaration_qualifier_list abstract_declarator
  	{% withNodeInfo $1 $ CDecl (reverse $1) [(Just (reverseDeclr $2), Nothing, Nothing)] }

  | declaration_qualifier_list identifier_declarator attrs_opt
  	{% withNodeInfo $1 $ CDecl (reverse $1) [(Just (reverseDeclr $! appendDeclrAttrs $3 $2), Nothing, Nothing)] }

  | type_specifier
  	{% withNodeInfo $1 $ CDecl $1 [] }

  | type_specifier abstract_declarator
  	{% withNodeInfo $1 $ CDecl $1 [(Just (reverseDeclr $2), Nothing, Nothing)] }

  | type_specifier identifier_declarator attrs_opt
  	{% withNodeInfo $1 $ CDecl $1 [(Just (reverseDeclr $! appendDeclrAttrs $3 $2), Nothing, Nothing)] }

  | type_specifier parameter_typedef_declarator attrs_opt
  	{% withNodeInfo $1 $ CDecl $1 [(Just (reverseDeclr $! appendDeclrAttrs $3 $2), Nothing, Nothing)] }

  | type_qualifier_list
  	{% withNodeInfo $1 $ CDecl (liftTypeQuals $1) [] }
  | type_qualifier_list attr
  	{% withNodeInfo $1 $ CDecl (liftTypeQuals $1 ++ liftCAttrs $2) [] }

  | type_qualifier_list abstract_declarator
  	{% withNodeInfo $1 $ CDecl (liftTypeQuals $1) [(Just (reverseDeclr $2), Nothing, Nothing)] }

  | type_qualifier_list identifier_declarator attrs_opt
  	{% withNodeInfo $1 $ CDecl (liftTypeQuals $1) [(Just (reverseDeclr$ appendDeclrAttrs $3 $2), Nothing, Nothing)] }


identifier_list :: { Reversed [Ident] }
identifier_list
  : ident				{ singleton $1 }
  | identifier_list ',' ident		{ $1 `snoc` $3 }


-- parse C type name (C99 6.7.6)
--
type_name :: { CDecl }
type_name
  :  type_specifier
  	{% withNodeInfo $1 $ CDecl $1 [] }

  |  type_specifier abstract_declarator
  	{% withNodeInfo $1 $ CDecl $1 [(Just (reverseDeclr $2), Nothing, Nothing)] }

  |  type_qualifier_list attr
  	{% withNodeInfo $1 $ CDecl (liftTypeQuals $1 ++ liftCAttrs $2) [] }

  |  type_qualifier_list abstract_declarator
  	{% withNodeInfo $1 $ CDecl (liftTypeQuals $1) [(Just (reverseDeclr $2), Nothing, Nothing)] }

-- parse C abstract declarator (C99 6.7.6)
--
-- postfix starts with '('
-- postfixing starts with '(' or '['
-- unary start with '*'
abstract_declarator :: { CDeclrR }
abstract_declarator
  : unary_abstract_declarator	    { $1 }
  | postfix_abstract_declarator		{ $1 }
  | postfixing_abstract_declarator  { $1 emptyDeclr }

--
-- FIXME
--  | postfixing_abstract_declarator attrs_opt	{ $1 emptyDeclr }


postfixing_abstract_declarator :: { CDeclrR -> CDeclrR }
postfixing_abstract_declarator
  : array_abstract_declarator
  	{ $1 }

  | '(' parameter_type_list ')'
  	{% withNodeInfo $1 $ \at declr -> case $2 of
             (params, variadic) -> funDeclr declr (Right (params,variadic)) [] at }


-- * TODO: Note that we recognise but ignore the C99 static keyword (see C99 6.7.5.3)
--
-- * TODO: We do not distinguish in the AST between incomplete array types and
-- complete variable length arrays ([ '*' ] means the latter). (see C99 6.7.5.2)
--
array_abstract_declarator :: { CDeclrR -> CDeclrR }
array_abstract_declarator
  : postfix_array_abstract_declarator
  	{ $1 }

  | array_abstract_declarator postfix_array_abstract_declarator
  	{ \decl -> $2 ($1 decl) }

--
-- TODO: record static
postfix_array_abstract_declarator :: { CDeclrR -> CDeclrR }
postfix_array_abstract_declarator
  : '[' assignment_expression_opt ']'
  	{% withNodeInfo $1 $ \at declr -> arrDeclr declr [] False False $2 at }

  | '[' attrs assignment_expression_opt ']'
  	{% withAttributePF $1 $2 $ \at declr -> arrDeclr declr [] False False $3 at }

  | '[' type_qualifier_list assignment_expression_opt ']'
  	{% withNodeInfo $1 $ \at declr -> arrDeclr declr (reverse $2) False False $3 at }

  | '[' type_qualifier_list attrs assignment_expression_opt ']'
  	{% withAttributePF $1 $3 $ \at declr -> arrDeclr declr (reverse $2) False False $4 at }

  | '[' static attrs_opt assignment_expression ']'
  	{% withAttributePF $1 $3 $ \at declr -> arrDeclr declr [] False True (Just $4) at }

  | '[' static type_qualifier_list attrs_opt assignment_expression ']'
  	{% withAttributePF $1 $4 $ \at declr -> arrDeclr declr (reverse $3) False True (Just $5) at }

  | '[' type_qualifier_list attrs_opt static attrs_opt assignment_expression ']'
  	{% withAttributePF $1 ($3 ++ $5) $ \at declr -> arrDeclr declr (reverse $2) False True  (Just $6) at }

  | '[' '*' attrs_opt ']'
  	{% withAttributePF $1 $3 $ \at declr -> arrDeclr declr [] True False Nothing at }
  | '[' attrs '*' attrs_opt ']'
  	{% withAttributePF $1 ($2 ++ $4) $ \at declr -> arrDeclr declr [] True False Nothing at }

  | '[' type_qualifier_list '*' attrs_opt ']'
  	{% withAttributePF $1 $4 $ \at declr -> arrDeclr declr (reverse $2) True False Nothing at }
  | '[' type_qualifier_list attrs '*' attrs_opt ']'
  	{% withAttributePF $1 ($3 ++ $5) $ \at declr -> arrDeclr declr (reverse $2) True False Nothing at }

unary_abstract_declarator :: { CDeclrR }
unary_abstract_declarator
  : '*'
  	{% withNodeInfo $1 $ ptrDeclr emptyDeclr [] }

  | '*' type_qualifier_list attrs_opt
  	{% withAttribute $1 $3 $ ptrDeclr emptyDeclr (reverse $2)  }

  | '*' abstract_declarator
  	{% withNodeInfo $1 $ ptrDeclr $2 [] }

  | '*' type_qualifier_list abstract_declarator
  	{% withNodeInfo $1 $ ptrDeclr $3 (reverse $2) }

  | '*' attrs
  	{% withAttribute $1 $2 $ ptrDeclr emptyDeclr [] }
  | '*' attrs abstract_declarator
  	{% withAttribute $1 $2 $ ptrDeclr $3 [] }

-- postfix_ad starts with '(', postfixing with '(' or '[', unary_abstract starts with '*'
postfix_abstract_declarator :: { CDeclrR }
postfix_abstract_declarator
  : '(' unary_abstract_declarator ')'					{ $2 }
  | '(' postfix_abstract_declarator ')'					{ $2 }
  | '(' postfixing_abstract_declarator ')'				{ $2 emptyDeclr }
  | '(' unary_abstract_declarator ')' postfixing_abstract_declarator	{ $4 $2 }

-- FIX 0700
  | '(' attrs unary_abstract_declarator ')'				     	{ appendDeclrAttrs $2 $3 }
  | '(' attrs postfix_abstract_declarator ')'					{ appendDeclrAttrs $2 $3 }
  | '(' attrs postfixing_abstract_declarator ')'				{ appendDeclrAttrs $2 ($3 emptyDeclr) }
  | '(' attrs unary_abstract_declarator ')' postfixing_abstract_declarator	{ appendDeclrAttrs $2 ($5 $3) }
  | postfix_abstract_declarator attr						    { appendDeclrAttrs $2 $1 }


-- parse C initializer (C99 6.7.8)
--
initializer :: { CInit }
initializer
  : assignment_expression		{% withNodeInfo $1 $ CInitExpr $1 }
  | '{' initializer_list '}'		{% withNodeInfo $1 $ CInitList (reverse $2) }
  | '{' initializer_list ',' '}'	{% withNodeInfo $1 $ CInitList (reverse $2) }


initializer_opt :: { Maybe CInit }
initializer_opt
  : {- empty -}			{ Nothing }
  | '=' initializer		{ Just $2 }


initializer_list :: { Reversed CInitList }
initializer_list
  : {- empty -}						{ empty }
  | initializer						{ singleton ([],$1) }
  | designation initializer				{ singleton ($1,$2) }
  | initializer_list ',' initializer			{ $1 `snoc` ([],$3) }
  | initializer_list ',' designation initializer	{ $1 `snoc` ($3,$4) }


-- designation
--
-- * GNU extensions:
--     old style member designation: 'ident :'
--     array range designation
--
designation :: { [CDesignator] }
designation
  : designator_list '='		{ reverse $1 }
  | identifier ':'		{% withNodeInfo $1 $ \at -> [CMemberDesig $1 at] }
  | array_designator		{ [$1] }


designator_list :: { Reversed [CDesignator] }
designator_list
 : designator				{ singleton $1 }
 | designator_list designator		{ $1 `snoc` $2 }


designator :: { CDesignator }
designator
  : '[' constant_expression ']'		{% withNodeInfo $1 $ CArrDesig $2 }
  | '.' identifier			{% withNodeInfo $1 $ CMemberDesig $2 }
  | array_designator			{ $1 }


array_designator :: { CDesignator }
array_designator
  : '[' constant_expression "..." constant_expression ']'
  	{% withNodeInfo $1 $ CRangeDesig $2 $4 }


-- parse C primary expression (C99 6.5.1)
--
-- We cannot use a typedef name as a variable
--
-- * GNU extensions:
--     allow a compound statement as an expression
--     __builtin_va_arg
--     __builtin_offsetof
--     __builtin_types_compatible_p
primary_expression :: { CExpr }
primary_expression
  : ident		       {% withNodeInfo $1 $ CVar $1 }
  | constant	  	 { CConst $1 }
  | string_literal { CConst (liftStrLit $1) }
  | '(' expression ')'	{ $2 }

  -- GNU extensions
  | '(' compound_statement ')'
  	{% withNodeInfo $1 $ CStatExpr $2 }

  | "__builtin_va_arg" '(' assignment_expression ',' type_name ')'
  	{% withNodeInfo $1 $ CBuiltinExpr . CBuiltinVaArg $3 $5 }

  | "__builtin_offsetof" '(' type_name ',' offsetof_member_designator ')'
  	{% withNodeInfo $1 $ CBuiltinExpr . CBuiltinOffsetOf $3 (reverse $5) }

  | "__builtin_types_compatible_p" '(' type_name ',' type_name ')'
  	{% withNodeInfo $1 $ CBuiltinExpr . CBuiltinTypesCompatible $3 $5 }


offsetof_member_designator :: { Reversed [CDesignator] }
offsetof_member_designator
  : identifier						                        {% withNodeInfo $1 $ singleton . CMemberDesig $1 }
  | offsetof_member_designator '.' identifier		  {% withNodeInfo $3 $ ($1 `snoc`) . CMemberDesig $3 }
  | offsetof_member_designator '[' expression ']'	{% withNodeInfo $3 $ ($1 `snoc`) . CArrDesig $3 }


-- parse C postfix expression (C99 6.5.2)
--
postfix_expression :: { CExpr }
postfix_expression
  : primary_expression
  	{ $1 }

  | postfix_expression '[' expression ']'
  	{% withNodeInfo $1 $ CIndex $1 $3 }

  | postfix_expression '(' ')'
  	{% withNodeInfo $1 $ CCall $1 [] }

  | postfix_expression '(' argument_expression_list ')'
  	{% withNodeInfo $1 $ CCall $1 (reverse $3) }

  | postfix_expression '.' identifier
  	{% withNodeInfo $1 $ CMember $1 $3 False }

  | postfix_expression "->" identifier
  	{% withNodeInfo $1 $ CMember $1 $3 True }

  | postfix_expression "++"
  	{% withNodeInfo $1 $ CUnary CPostIncOp $1 }

  | postfix_expression "--"
  	{% withNodeInfo $1 $ CUnary CPostDecOp $1 }

  | '(' type_name ')' '{' initializer_list '}'
  	{% withNodeInfo $1 $ CCompoundLit $2 (reverse $5) }

  | '(' type_name ')' '{' initializer_list ',' '}'
  	{% withNodeInfo $1 $ CCompoundLit $2 (reverse $5) }


argument_expression_list :: { Reversed [CExpr] }
argument_expression_list
  : assignment_expression				{ singleton $1 }
  | argument_expression_list ',' assignment_expression	{ $1 `snoc` $3 }


-- parse C unary expression (C99 6.5.3)
--
-- * GNU extensions:
--     'alignof' expression or type
--     '__real' and '__imag' expression
--     '__extension__' to suppress warnings about extensions
--     allow taking address of a label with: && label
--
unary_expression :: { CExpr }
unary_expression
  : postfix_expression			{ $1 }
  | "++" unary_expression		{% withNodeInfo $1 $ CUnary CPreIncOp $2 }
  | "--" unary_expression		{% withNodeInfo $1 $ CUnary CPreDecOp $2 }
  | "__extension__" cast_expression	{ $2 }
  | unary_operator cast_expression	{% withNodeInfo $1 $ CUnary (unL $1) $2 }
  | sizeof unary_expression		{% withNodeInfo $1 $ CSizeofExpr $2 }
  | sizeof '(' type_name ')'		{% withNodeInfo $1 $ CSizeofType $3 }
  -- GNU: alignof, complex and && extension
  | alignof unary_expression		{% withNodeInfo $1 $ CAlignofExpr $2 }
  | alignof '(' type_name ')'		{% withNodeInfo $1 $ CAlignofType $3 }
  | "__real__" unary_expression    {% withNodeInfo $1 $ CComplexReal $2 }
  | "__imag__" unary_expression    {% withNodeInfo $1 $ CComplexImag $2 }
  | "&&" identifier			{% withNodeInfo $1 $ CLabAddrExpr $2 }


unary_operator :: { Located CUnaryOp }
unary_operator
  : '&'		{ L CAdrOp  (posOf $1) }
  | '*'		{ L CIndOp  (posOf $1) }
  | '+'		{ L CPlusOp (posOf $1) }
  | '-'		{ L CMinOp  (posOf $1) }
  | '~'		{ L CCompOp (posOf $1) }
  | '!'		{ L CNegOp  (posOf $1) }


-- parse C cast expression (C99 6.5.4)
--
cast_expression :: { CExpr }
cast_expression
  : unary_expression			{ $1 }
  | '(' type_name ')' cast_expression	{% withNodeInfo $1 $ CCast $2 $4 }


-- parse C multiplicative expression (C99 6.5.5)
--
multiplicative_expression :: { CExpr }
multiplicative_expression
  : cast_expression
  	{ $1 }

  | multiplicative_expression '*' cast_expression
  	{% withNodeInfo $1 $ CBinary CMulOp $1 $3 }

  | multiplicative_expression '/' cast_expression
  	{% withNodeInfo $1 $ CBinary CDivOp $1 $3 }

  | multiplicative_expression '%' cast_expression
  	{% withNodeInfo $1 $ CBinary CRmdOp $1 $3 }


-- parse C additive expression (C99 6.5.6)
--
additive_expression :: { CExpr }
additive_expression
  : multiplicative_expression
  	{ $1 }

  | additive_expression '+' multiplicative_expression
  	{% withNodeInfo $1 $ CBinary CAddOp $1 $3 }

  | additive_expression '-' multiplicative_expression
  	{% withNodeInfo $1 $ CBinary CSubOp $1 $3 }


-- parse C shift expression (C99 6.5.7)
--
shift_expression :: { CExpr }
shift_expression
  : additive_expression
  	{ $1 }

  | shift_expression "<<" additive_expression
  	{% withNodeInfo $1 $ CBinary CShlOp $1 $3 }

  | shift_expression ">>" additive_expression
  	{% withNodeInfo $1 $ CBinary CShrOp $1 $3 }


-- parse C relational expression (C99 6.5.8)
--
relational_expression :: { CExpr }
relational_expression
  : shift_expression
  	{ $1 }

  | relational_expression '<' shift_expression
  	{% withNodeInfo $1 $ CBinary CLeOp $1 $3 }

  | relational_expression '>' shift_expression
  	{% withNodeInfo $1 $ CBinary CGrOp $1 $3 }

  | relational_expression "<=" shift_expression
  	{% withNodeInfo $1 $ CBinary CLeqOp $1 $3 }

  | relational_expression ">=" shift_expression
  	{% withNodeInfo $1 $ CBinary CGeqOp $1 $3 }


-- parse C equality expression (C99 6.5.9)
--
equality_expression :: { CExpr }
equality_expression
  : relational_expression
  	{ $1 }

  | equality_expression "==" relational_expression
  	{% withNodeInfo $1 $ CBinary CEqOp  $1 $3 }

  | equality_expression "!=" relational_expression
  	{% withNodeInfo $1 $ CBinary CNeqOp $1 $3 }


-- parse C bitwise and expression (C99 6.5.10)
--
and_expression :: { CExpr }
and_expression
  : equality_expression
  	{ $1 }

  | and_expression '&' equality_expression
  	{% withNodeInfo $1 $ CBinary CAndOp $1 $3 }


-- parse C bitwise exclusive or expression (C99 6.5.11)
--
exclusive_or_expression :: { CExpr }
exclusive_or_expression
  : and_expression
  	{ $1 }

  | exclusive_or_expression '^' and_expression
  	{% withNodeInfo $1 $ CBinary CXorOp $1 $3 }


-- parse C bitwise or expression (C99 6.5.12)
--
inclusive_or_expression :: { CExpr }
inclusive_or_expression
  : exclusive_or_expression
  	{ $1 }

  | inclusive_or_expression '|' exclusive_or_expression
  	{% withNodeInfo $1 $ CBinary COrOp $1 $3 }


-- parse C logical and expression (C99 6.5.13)
--
logical_and_expression :: { CExpr }
logical_and_expression
  : inclusive_or_expression
  	{ $1 }

  | logical_and_expression "&&" inclusive_or_expression
  	{% withNodeInfo $1 $ CBinary CLndOp $1 $3 }


-- parse C logical or expression (C99 6.5.14)
--
logical_or_expression :: { CExpr }
logical_or_expression
  : logical_and_expression
  	{ $1 }

  | logical_or_expression "||" logical_and_expression
  	{% withNodeInfo $1 $ CBinary CLorOp $1 $3 }


-- parse C conditional expression (C99 6.5.15)
--
-- * GNU extensions:
--     omitting the `then' part
conditional_expression :: { CExpr }
conditional_expression
  : logical_or_expression
  	{ $1 }

  | logical_or_expression '?' expression ':' conditional_expression
  	{% withNodeInfo $1 $ CCond $1 (Just $3) $5 }

  | logical_or_expression '?' ':' conditional_expression
  	{% withNodeInfo $1 $ CCond $1 Nothing $4 }


-- parse C assignment expression (C99 6.5.16)
--
-- * NOTE: LHS of assignment is more restricted than in gcc.
--         `x ? y : z = 3' parses in gcc as `(x ? y : z) = 3',
--         but `x ? y : z' is not an unary expression.
assignment_expression :: { CExpr }
assignment_expression
  : conditional_expression
  	{ $1 }

  | unary_expression assignment_operator assignment_expression
  	{% withNodeInfo $1 $ CAssign (unL $2) $1 $3 }


assignment_operator :: { Located CAssignOp }
assignment_operator
  : '='			{ L CAssignOp (posOf $1) }
  | "*="		{ L CMulAssOp (posOf $1) }
  | "/="		{ L CDivAssOp (posOf $1) }
  | "%="		{ L CRmdAssOp (posOf $1) }
  | "+="		{ L CAddAssOp (posOf $1) }
  | "-="		{ L CSubAssOp (posOf $1) }
  | "<<="		{ L CShlAssOp (posOf $1) }
  | ">>="		{ L CShrAssOp (posOf $1) }
  | "&="		{ L CAndAssOp (posOf $1) }
  | "^="		{ L CXorAssOp (posOf $1) }
  | "|="		{ L COrAssOp  (posOf $1) }


-- parse C expression (C99 6.5.17)
--
expression :: { CExpr }
expression
  : assignment_expression
  	{ $1 }

  | assignment_expression ',' comma_expression
  	{% let es = reverse $3 in withNodeInfo es $ CComma ($1:es) }

comma_expression :: { Reversed [CExpr] }
comma_expression
  : assignment_expression			{ singleton $1 }
  | comma_expression ',' assignment_expression	{ $1 `snoc` $3 }


-- The following was used for clarity
expression_opt :: { Maybe CExpr }
expression_opt
  : {- empty -}		{ Nothing }
  | expression		{ Just $1 }


-- The following was used for clarity
assignment_expression_opt :: { Maybe CExpr }
assignment_expression_opt
  : {- empty -}				{ Nothing }
  | assignment_expression		{ Just $1 }


-- parse C constant expression (C99 6.6)
--
constant_expression :: { CExpr }
constant_expression
  : conditional_expression			{ $1 }


-- parse C constants
--
constant :: { CConst }
constant
  : cint	  {% withNodeInfo $1 $ case $1 of CTokILit _ i -> CIntConst i }
  | cchar	  {% withNodeInfo $1 $ case $1 of CTokCLit _ c -> CCharConst c }
  | cfloat	{% withNodeInfo $1 $ case $1 of CTokFLit _ f -> CFloatConst f }


string_literal :: { CStrLit }
string_literal
  : cstr
  	{% withNodeInfo $1 $ case $1 of CTokSLit _ s -> CStrLit s }

  | cstr string_literal_list
  	{% withNodeInfo $1 $ case $1 of CTokSLit _ s -> CStrLit (concatCStrings (s : reverse $2)) }


string_literal_list :: { Reversed [CString] }
string_literal_list
  : cstr			{ case $1 of CTokSLit _ s -> singleton s }
  | string_literal_list cstr	{ case $2 of CTokSLit _ s -> $1 `snoc` s }


identifier :: { Ident }
identifier
  : ident		{ $1 }
  | tyident		{ $1 }


-- parse GNU C attribute annotation
attrs_opt ::	{ [CAttr] }
attrs_opt
  : {- empty -}						{ [] }
  | attrs         				{ $1 }

-- GNU C attribute annotation
attrs :: { [CAttr] }
attrs
  : attr						{ $1 }
  | attrs attr	    { $1 ++ $2 }

attr :: { [CAttr] }
attr
  : "__attribute__" '(' '(' attribute_list ')' ')'	{ reverse $4 }

attribute_list :: { Reversed [CAttr] }
  : attribute						          { case $1 of Nothing -> empty; Just attr -> singleton attr }
  | attribute_list ',' attribute	{ (maybe id (flip snoc) $3) $1 }


attribute :: { Maybe CAttr }
attribute
  : {- empty -}						         { Nothing }
  | ident						               {% withNodeInfo $1 $ Just . CAttr $1  [] }
  | const						               {% withNodeInfo $1 $ Just . CAttr (internalIdent "const") [] }
  | ident '(' attribute_params ')' {% withNodeInfo $1 $ Just . CAttr $1 (reverse $3) }
  | ident '(' ')'					         {% withNodeInfo $1 $ Just . CAttr $1 [] }

-- OS X 10.9 (Mavericks) makes use of more liberal attribute syntax
-- that includes assignment-like expressions referencing version
-- numbers.

attribute_params :: { Reversed [CExpr] }
attribute_params
  : constant_expression					              { singleton $1 }
  | unary_expression assignment_operator unary_expression { Reversed [] }
  | attribute_params ',' constant_expression	{ $1 `snoc` $3 }
  | attribute_params ',' unary_expression assignment_operator unary_expression { $1 }

{

--  sometimes it is neccessary to reverse an unreversed list
reverseList :: [a] -> Reversed [a]
reverseList = Reversed . List.reverse

-- We occasionally need things to have a location when they don't naturally
-- have one built in as tokens and most AST elements do.
--
data Located a = L !a !Position

unL :: Located a -> a
unL (L a pos) = a

instance Pos (Located a) where
  posOf (L _ pos) = pos

-- FIXME: the next 3 inlines here increase the object file size by  70%
-- Check whether the speed win is worth it
{-# INLINE withNodeInfo #-}
withNodeInfo :: Pos node => node -> (NodeInfo -> a) -> P a
withNodeInfo node mkAttrNode = do
  name <- getNewName
  lastTok <- getSavedToken
  let firstPos = posOf node
  let attrs = mkNodeInfo' firstPos (posLenOfTok $! lastTok) name
  attrs `seq` return (mkAttrNode attrs)

{-# INLINE withLength #-}
withLength :: NodeInfo -> (NodeInfo -> a) -> P a
withLength nodeinfo mkAttrNode = do
  lastTok <- getSavedToken
  let firstPos = posOfNode nodeinfo
  let attrs = mkNodeInfo' firstPos (posLenOfTok $! lastTok)
              (maybe (error "nameOfNode") id (nameOfNode nodeinfo))
  attrs `seq` return (mkAttrNode attrs)

data CDeclrR = CDeclrR (Maybe Ident) (Reversed [CDerivedDeclr]) (Maybe CStrLit) [CAttr] NodeInfo
reverseDeclr :: CDeclrR -> CDeclr
reverseDeclr (CDeclrR ide reversedDDs asmname cattrs at)
    = CDeclr ide (reverse reversedDDs) asmname cattrs at
instance CNode (CDeclrR) where
    nodeInfo (CDeclrR _ _ _ _ n) = n
instance Pos (CDeclrR) where
    posOf (CDeclrR _ _ _ _ n) = posOf n

{-# INLINE withAttribute #-}
withAttribute :: Pos node => node -> [CAttr] -> (NodeInfo -> CDeclrR) -> P CDeclrR
withAttribute node cattrs mkDeclrNode = do
  name <- getNewName
  let attrs = mkNodeInfo (posOf node) name
  let newDeclr = appendDeclrAttrs cattrs $ mkDeclrNode attrs
  attrs `seq` newDeclr `seq` return newDeclr

-- postfixing variant
{-# INLINE withAttributePF #-}
withAttributePF :: Pos node => node -> [CAttr] -> (NodeInfo -> CDeclrR -> CDeclrR) -> P (CDeclrR -> CDeclrR)
withAttributePF node cattrs mkDeclrCtor = do
  name <- getNewName
  let attrs = mkNodeInfo (posOf node) name
  let newDeclr = appendDeclrAttrs cattrs . mkDeclrCtor attrs
  attrs `seq` newDeclr `seq` return newDeclr

-- add top level attributes for a declarator.
--
-- In the following example
--
-- > int declr1, __attribute__((a1)) * __attribute__((a2)) y() __asm__("$" "y") __attribute__((a3));
--
-- the attributes `a1' and `a3' are top-level attributes for y.
-- The (pseudo)-AST for the second declarator is
--
-- > CDeclr "y"
-- >        [CFunDeclr ..., CPtrDeclr __attribute__((a2)) ... ]
-- >        (asm "$y")
-- >        [__attribute__((a1)), __attribute__((a3)) ]
--
-- So assembler names and preceeding and trailing attributes are recorded in object declarator.
--
appendObjAttrs :: [CAttr] -> CDeclr -> CDeclr
appendObjAttrs newAttrs (CDeclr ident indirections asmname cAttrs at)
    = CDeclr ident indirections asmname (cAttrs ++ newAttrs) at
appendObjAttrsR :: [CAttr] -> CDeclrR -> CDeclrR
appendObjAttrsR newAttrs (CDeclrR ident indirections asmname cAttrs at)
    = CDeclrR ident indirections asmname (cAttrs ++ newAttrs) at

setAsmName :: Maybe CStrLit  -> CDeclrR -> P CDeclrR
setAsmName mAsmName (CDeclrR ident indirections oldName cattrs at) =
    case combineName mAsmName oldName of
        Left (n1,n2)       -> failP (posOf n2) ["Duplicate assembler name: ",showName n1,showName n2]
        Right newName      -> return $ CDeclrR ident indirections newName cattrs at
  where
  combineName Nothing Nothing = Right Nothing
  combineName Nothing oldname@(Just _)  = Right oldname
  combineName newname@(Just _) Nothing  = Right newname
  combineName (Just n1) (Just n2) = Left (n1,n2)
  showName (CStrLit cstr _) = show cstr

withAsmNameAttrs :: (Maybe CStrLit, [CAttr]) -> CDeclrR -> P CDeclrR
withAsmNameAttrs (mAsmName, newAttrs) declr = setAsmName mAsmName (appendObjAttrsR newAttrs declr)

appendDeclrAttrs :: [CAttr] -> CDeclrR -> CDeclrR
appendDeclrAttrs newAttrs (CDeclrR ident (Reversed []) asmname cattrs at)
    = CDeclrR ident empty asmname (cattrs ++ newAttrs) at
appendDeclrAttrs newAttrs (CDeclrR ident (Reversed (x:xs)) asmname cattrs at)
    = CDeclrR ident (Reversed (appendAttrs x : xs)) asmname cattrs at where
    appendAttrs (CPtrDeclr typeQuals at)           = CPtrDeclr (typeQuals ++ map CAttrQual newAttrs) at
    appendAttrs (CArrDeclr typeQuals arraySize at) = CArrDeclr (typeQuals ++ map CAttrQual newAttrs) arraySize at
    appendAttrs (CFunDeclr parameters cattrs at)   = CFunDeclr parameters (cattrs ++ newAttrs) at

ptrDeclr :: CDeclrR -> [CTypeQual] -> NodeInfo -> CDeclrR
ptrDeclr (CDeclrR ident derivedDeclrs asmname cattrs dat) tyquals at
    = CDeclrR ident (derivedDeclrs `snoc` CPtrDeclr tyquals at) asmname cattrs dat
funDeclr :: CDeclrR -> (Either [Ident] ([CDecl],Bool)) -> [CAttr] -> NodeInfo -> CDeclrR
funDeclr (CDeclrR ident derivedDeclrs asmname dcattrs dat) params cattrs at
    = CDeclrR ident (derivedDeclrs `snoc` CFunDeclr params cattrs at) asmname dcattrs dat
arrDeclr :: CDeclrR -> [CTypeQual] -> Bool -> Bool -> Maybe CExpr -> NodeInfo -> CDeclrR
arrDeclr (CDeclrR ident derivedDeclrs asmname cattrs dat) tyquals var_sized static_size size_expr_opt at
    = arr_sz `seq` ( CDeclrR ident (derivedDeclrs `snoc` CArrDeclr tyquals arr_sz at) asmname cattrs dat )
    where
    arr_sz = case size_expr_opt of
                 Just e  -> CArrSize static_size e
                 Nothing -> CNoArrSize var_sized

liftTypeQuals :: Reversed [CTypeQual] -> [CDeclSpec]
liftTypeQuals = map CTypeQual . reverse

-- lift CAttrs to DeclSpecs
--
liftCAttrs :: [CAttr] -> [CDeclSpec]
liftCAttrs = map (CTypeQual . CAttrQual)

-- when we parsed (decl_spec_1,...,decl_spec_n,attrs), add the __attributes__s to the declspec list
-- needs special care when @decl_spec_n@ is a SUE definition
addTrailingAttrs :: Reversed [CDeclSpec] -> [CAttr] -> Reversed [CDeclSpec]
addTrailingAttrs declspecs new_attrs =
    case viewr declspecs of
        (specs_init, CTypeSpec (CSUType (CStruct tag name (Just def) def_attrs su_node) node))
            -> (specs_init `snoc` CTypeSpec (CSUType (CStruct tag name (Just def) (def_attrs ++ new_attrs) su_node) node))
        (specs_init, CTypeSpec (CEnumType (CEnum name (Just def) def_attrs e_node) node))
            -> (specs_init `snoc` CTypeSpec (CEnumType (CEnum name (Just def) (def_attrs ++ new_attrs) e_node) node))
        _ -> declspecs `rappend` (liftCAttrs new_attrs)

-- convenient instance, the position of a list of things is the position of
-- the first thing in the list
--
instance Pos a => Pos [a] where
  posOf (x:_) = posOf x

instance Pos a => Pos (Reversed a) where
  posOf (Reversed x) = posOf x

emptyDeclr :: CDeclrR
emptyDeclr       = CDeclrR Nothing empty Nothing [] undefNode
mkVarDeclr :: Ident -> NodeInfo -> CDeclrR
mkVarDeclr ident = CDeclrR (Just ident) empty Nothing []

-- Take the identifiers and use them to update the typedef'ed identifier set
-- if the decl is defining a typedef then we add it to the set,
-- if it's a var decl then that shadows typedefed identifiers
--
doDeclIdent :: [CDeclSpec] -> CDeclrR -> P ()
doDeclIdent declspecs (CDeclrR mIdent _ _ _ _) =
  case mIdent of
    Nothing -> return ()
    Just ident | any iypedef declspecs -> addTypedef ident
               | otherwise             -> shadowTypedef ident

  where iypedef (CStorageSpec (CTypedef _)) = True
        iypedef _                           = False

doFuncParamDeclIdent :: CDeclr -> P ()
doFuncParamDeclIdent (CDeclr _ (CFunDeclr params _ _ : _) _ _ _) =
  sequence_
    [ case getCDeclrIdent declr of
        Nothing -> return ()
        Just ident -> shadowTypedef ident
    | CDecl _ dle _  <- either (const []) fst params
    , (Just declr, _, _) <- dle ]
doFuncParamDeclIdent _ = return ()

-- extract all identifiers
getCDeclrIdent :: CDeclr -> Maybe Ident
getCDeclrIdent (CDeclr mIdent _ _ _ _) = mIdent

happyError :: P a
happyError = parseError

-- * public interface

-- | @parseC input initialPos@ parses the given preprocessed C-source input and returns the AST or a list of parse errors.
parseC :: InputStream -> Position -> Either ParseError CTranslUnit
parseC input initialPosition =
  fmap fst $ execParser translUnitP input initialPosition builtinTypeNames (namesStartingFrom 0)

-- | @translUnitP@ provides a parser for a complete C translation unit, i.e. a list of external declarations.
translUnitP :: P CTranslUnit
translUnitP = translation_unit
-- | @extDeclP@ provides a parser for an external (file-scope) declaration
extDeclP :: P CExtDecl
extDeclP = external_declaration
-- | @statementP@ provides a parser for C statements
statementP :: P CStat
statementP = statement
-- | @expressionP@ provides a parser for C expressions
expressionP :: P CExpr
expressionP = expression
}
