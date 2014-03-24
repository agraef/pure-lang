-- Dump an AST from Language.C into a Pure term
-- 
-- Takes a single filename on the input, dumps the Pure term on stdout
--
-- This is awfully tedious. I'm sure there's some way to do this with "scrap
-- your boilerplate" but I have no desire to learn that stuff. 
-- 
-- Not everything is dumped (not even close.) Omitted subtrees are marked by a
-- "Pass" term.

import Data.List
import Language.C
import Language.C.System.GCC
import Language.C.Syntax.AST
import Language.C.Data.Ident
import System.Environment

main = 
  getArgs >>= parseFile >>= return.dump >>= putStrLn

parseFile :: [String] -> IO CTranslUnit
parseFile (arg:args) =
  do
  parseResult <- parseCFile (newGCC arg) Nothing (init args) (last args)
  case parseResult of 
    Left err -> error $ show err
    Right ast -> return $ ast


--Dump is basically Show, except we filter out some noise, and ensure that the
--result is a valid Pure term. Specifically, record syntax is not allowed.
class Dump n where
  dump :: n -> String
  -- Output enclosed in () or [] if and only if it is not a single token.

instance Dump Bool where
  dump True  = "1"
  dump False = "0"

instance Dump a => Dump (Maybe a) where
  dump (Just a) = parens $ "Just " ++ dump a
  dump Nothing = "Nothing"

instance (Dump a, Dump b) => Dump (Either a b) where
  dump (Left a) = parens $ "Left " ++ dump a
  dump (Right b) = parens $ "Right " ++ dump b

instance (Dump a, Dump b) => Dump (a,b) where
  dump (a,b) =  parens $ dump a++","++dump b

instance (Dump a, Dump b, Dump c) => Dump (a,b,c) where
  dump (a,b,c) = parens $ dump a++","++dump b++","++dump c

instance Dump Ident where
  dump (Ident name _ _) = "\""++name++"\""

instance (Dump a) => Dump [a] where
  dump = dumpList

dumpList :: Dump n => [n] -> String
dumpList xs = "[" ++ intercalate "," (map dump xs) ++ "]"

dumpListNewlines :: Dump n => [n] -> String
dumpListNewlines xs = "\n[" ++ intercalate "\n," (map dump xs) ++ "\n]"

parens x = "(" ++ x ++ ")"

pass = "Pass" --place holder for stuff not handled yet

-- The rest of this is quoted from Language.C.AST 
-----------------------------------------------------------------------------
--
-- Module      :  Language.C.Syntax.AST
-- Copyright   :  (c) [1999..2007] Manuel M T Chakravarty
--                (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Abstract syntax of C source and header files.
--
--  The tree structure is based on the grammar in Appendix A of K&R.  The
--  abstract syntax simplifies the concrete syntax by merging similar concrete
--  constructs into a single type of abstract tree structure: declarations are
--  merged with structure declarations, parameter declarations and type names,
--  and declarators are merged with abstract declarators.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988. The AST supports all
--  of C99 <http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf> and several
--  GNU extensions <http://gcc.gnu.org/onlinedocs/gcc/C-Extensions.html>.
-----------------------------------------------------------------------------
-- Copyright (c) 1999-2008 Manuel M T Chakravarty 
--                         Duncan Coutts
--                         Benedikt Huber
-- Portions Copyright (c)  1989,  1990  James  A.  Roskind
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.


-- | Complete C tranlsation unit (C99 6.9, K&R A10)
--
-- A complete C translation unit, for example representing a C header or source file.
-- It consists of a list of external (i.e. toplevel) declarations.

-- data CTranslUnit = CTranslUnit [CExtDecl] NodeInfo

instance Dump CTranslUnit where
  dump (CTranslUnit cExtDecls _) = unlines $ map dump cExtDecls -- !!!

-- | External C declaration (C99 6.9, K&R A10)
--
-- Either a toplevel declaration, function definition or external assembler.

--data CExtDecl = CDeclExt CDecl
--              | CFDefExt CFunDef
--              | CAsmExt  CStrLit NodeInfo

instance Dump CExtDecl where
  dump (CDeclExt cDecl)    = parens . unwords $ ["CDeclExt", dump cDecl, cDeclFile cDecl]
  -- NOTE : The output deviates from the Language.C format here.  We add an
  -- extra argument to the CDeclExt constructor, which is the filename.
  dump (CFDefExt cFunDef)  = parens $ "CFDefExt "++ dump cFunDef
  -- The version of CAsmExt in the repository has an additional second
  -- argument. Uncomment this if needed.
  --dump (CAsmExt cStrLit _) = parens $ "CAsmExt " ++ dump cStrLit
  dump (CAsmExt cStrLit) = parens $ "CAsmExt " ++ dump cStrLit

cDeclFile (CDecl _ _ nodeInfo) =
  case fileOfNode nodeInfo of
    filePath -> "\"" ++ filePath ++ "\""

-- | C function definition (C99 6.9.1, K&R A10.1)
--
-- A function definition is of the form @CFunDef specifiers declarator decllist? stmt@.
--
-- * @specifiers@ are the type and storage-class specifiers of the function.
--   The only storage-class specifiers allowed are /extern/ and /static/.
--
-- * The @declarator@ must be such that the declared identifier has /function type/.
--   The return type shall be void or an object type other than array type.
--
-- * The optional declaration list @decllist@ is for old-style function declarations.
--
-- * The statement @stmt@ is a compound statement.

-- data CFunDef = CFunDef [CDeclSpec]      -- type specifier and qualifier
--                        CDeclr           -- declarator
--                        [CDecl]          -- optional declaration list
--                        CStat            -- compound statement
--                        NodeInfo

instance Dump CFunDef where
  dump (CFunDef cDeclSpecs cDeclr cDecls cStat _) = pass




-- | C declarations (K&R A8, C99 6.7), including structure declarations, parameter
--   declarations and type names.
--
-- A declaration is of the form @CDecl specifiers init-declarator-list@, where the form of the declarator list's
--  elements depends on the kind of declaration:
--
-- 1) Toplevel declarations (K&R A8, C99 6.7 declaration)
--
--   * C99 requires that there is at least one specifier, though this is merely a syntactic restriction
--
--   * at most one storage class specifier is allowed per declaration
--
--   * the elements of the non-empty @init-declarator-list@ are of the form @(Just declr, init?, Nothing)@.
--      The declarator @declr@ has to be present and non-abstract and the initialization expression is
--      optional.
--
-- 2) Structure declarations (K&R A8.3, C99 6.7.2.1 struct-declaration)
--
--   Those are the declarations of a structure's members.
--
--   * do not allow storage specifiers
--
--   * in strict C99, the list of declarators has to be non-empty
--
--   * the elements of @init-declarator-list@ are either of the form @(Just declr, Nothing, size?)@,
--     representing a member with optional bit-field size, or of the form @(Nothing, Nothing, Just size)@,
--     for unnamed bitfields. @declr@ has to be non-abstract.
--
--   * no member of a structure shall have incomplete type
--
-- 3) Parameter declarations (K&R A8.6.3, C99 6.7.5 parameter-declaration)
--
--   * @init-declarator-list@ must contain at most one triple of the form @(Just declr, Nothing, Nothing)@,
--     i.e. consist of a single declarator, which is allowed to be abstract (i.e. unnamed).
--
-- 4) Type names (A8.8, C99 6.7.6)
--
--   * do not allow storage specifiers
--
--   * @init-declarator-list@ must contain at most one triple of the form @(Just declr, Nothing, Nothing)@.
--     where @declr@ is an abstract declarator (i.e. doesn't contain a declared identifier)
--

-- data CDecl = CDecl [CDeclSpec]          -- type specifier and qualifier, __attribute__
--                    [(Maybe CDeclr,      -- declarator (may be omitted)
--                      Maybe CInit,       -- optional initialize
--                      Maybe CExpr)]      -- optional size (const expr)
--                    NodeInfo

instance Dump CDecl where
  dump (CDecl cDeclSpecs maybes _) =
    parens . unwords $ ["CDecl" , dumpList cDeclSpecs , dumpList maybes]





-- | C declarator (K&R A8.5, C99 6.7.5) and abstract declarator (K&R A8.8, C99 6.7.6)
--
-- A declarator declares a single object, function, or type. It is always associated with
-- a declaration ('CDecl'), which specifies the declaration's type and the additional storage qualifiers and
-- attributes, which apply to the declared object.
--
-- A declarator is of the form @CDeclr name? indirections asm-name? attrs _@, where
-- @name@ is the name of the declared object (missing for abstract declarators),
-- @declquals@ is a set of additional declaration specifiers,
-- @asm-name@ is the optional assembler name and attributes is a set of
-- attrs is a set of @__attribute__@ annotations for the declared object.
--
-- @indirections@ is a set of pointer, array and function declarators, which modify the type of the declared object as
-- described below. If the /declaration/ specifies the non-derived type @T@,
-- and we have @indirections = [D1, D2, ..., Dn]@ than the declared object has type
-- @(D1 `indirect` (D2 `indirect` ...  (Dn `indirect` T)))@, where
--
--  * @(CPtrDeclr attrs) `indirect` T@ is /attributed pointer to T/
--
--  * @(CFunDeclr attrs) `indirect` T@ is /attributed function returning T/
--
--  * @(CArrayDeclr attrs) `indirect` T@ is /attributed array of elemements of type T/
--
-- Examples (simplified attributes):
--
--  * /x/ is an int
--
-- > int x;
-- > CDeclr "x" []
--
--  * /x/ is a restrict pointer to a const pointer to int
--
-- > const int * const * restrict x;
-- > CDeclr "x" [CPtrDeclr [restrict], CPtrDeclr [const]]
--
--  * /f/ is an function return a constant pointer to int
--
-- > int* const f();
-- > CDeclr "f" [CFunDeclr [],CPtrDeclr [const]]
--
--  * /f/ is a constant pointer to a function returning int
--
-- > int (* const f)(); ==>
-- > CDeclr "f" [CPtrDeclr [const], CFunDeclr []]

-- data CDeclr = CDeclr (Maybe Ident) [CDerivedDeclr] (Maybe CStrLit) [CAttr] NodeInfo

instance Dump CDeclr where
  dump (CDeclr mIdent cDerivedDeclrs mCStrLit cAttrs _) =
    parens . unwords $ 
      ["CDeclr"
      , dump mIdent
      , dumpList cDerivedDeclrs
      , dump mCStrLit
      , dumpList cAttrs 
      ]




-- | Derived declarators, see 'CDeclr'
--
-- Indirections are qualified using type-qualifiers and generic attributes, and additionally
--
--    * The size of an array is either a constant expression, variable length ('*') or missing; in the last case, the
--      type of the array is incomplete. The qualifier static is allowed for function arguments only, indicating that
--      the supplied argument is an array of at least the given size.
--
--    * New style parameter lists have the form @Right (declarations, isVariadic)@, old style parameter lists have the
--      form @Left (parameter-names)@

-- data CDerivedDeclr =
--               CPtrDeclr [CTypeQual] NodeInfo
--               -- ^ Pointer declarator @CPtrDeclr tyquals declr@
--             | CArrDeclr [CTypeQual] (CArrSize) NodeInfo
--               -- ^ Array declarator @CArrDeclr declr tyquals size-expr?@
--             | CFunDeclr (Either [Ident] ([CDecl],Bool)) [CAttr] NodeInfo
--               -- ^ Function declarator @CFunDeclr declr (old-style-params | new-style-params) c-attrs@

instance Dump CDerivedDeclr where
  dump (CPtrDeclr cTypeQuals _)          = parens $ "CPtrDeclr "++ dumpList cTypeQuals
  dump (CArrDeclr cTypeQuals cArrSize _) = parens $ "CArrDeclr "++ dumpList cTypeQuals++" "++ dump cArrSize
  dump (CFunDeclr e cAttrs _)            = parens $ "CFunDeclr "++ dump e ++ " " ++ dumpList cAttrs





-- | Size of an array

-- data CArrSize = CNoArrSize Bool     -- ^ @CUnknownSize isCompleteType@
--               | CArrSize Bool CExpr -- ^ @CArrSize isStatic expr@

instance Dump CArrSize where
  dump (CNoArrSize b)     = pass
  dump (CArrSize b cExpr) = pass




-- | C statement (K&R A9, C99 6.8)

-- data CStat = CLabel  Ident CStat [CAttr] NodeInfo  -- ^ An (attributed) label followed by a statement
--            | CCase CExpr CStat NodeInfo            -- ^ A statement of the form @case expr : stmt@
--            | CCases CExpr CExpr CStat NodeInfo     -- ^ A case range of the form @case lower ... upper : stmt@
--            | CDefault CStat NodeInfo               -- ^ The default case @default : stmt@
--            | CExpr (Maybe CExpr) NodeInfo
--            -- ^ A simple statement, that is in C: evaluating an expression with side-effects
--            --   and discarding the result.
--            | CCompound [Ident] [CBlockItem] NodeInfo    -- ^ compound statement @CCompound localLabels blockItems at@
--            | CIf CExpr CStat (Maybe CStat) NodeInfo     -- ^ conditional statement @CIf ifExpr thenStmt maybeElseStmt at@
--            | CSwitch CExpr CStat NodeInfo
--            -- ^ switch statement @CSwitch selectorExpr switchStmt@, where @switchStmt@ usually includes
--            -- /case/, /break/ and /default/ statements
--            | CWhile CExpr CStat Bool NodeInfo      -- ^ while or do-while statement @CWhile guard stmt isDoWhile at@
--            | CFor (Either (Maybe CExpr) CDecl)
--                   (Maybe CExpr)
--                   (Maybe CExpr)
--                   CStat
--                   NodeInfo
--            -- ^ for statement @CFor init expr-2 expr-3 stmt@, where @init@ is either a declaration or
--            -- initializing expression
--            | CGoto Ident NodeInfo            -- ^ goto statement @CGoto label@
--            | CGotoPtr CExpr NodeInfo         -- ^ computed goto @CGotoPtr labelExpr@
--            | CCont NodeInfo                  -- ^ continue statement
--            | CBreak    NodeInfo              -- ^ break statement
--            | CReturn (Maybe CExpr)NodeInfo   -- ^ return statement @CReturn returnExpr@
--            | CAsm CAsmStmt NodeInfo          -- ^ assembly statement

instance Dump CStat where
  dump cstat =
    case cstat of
      CLabel ident cStat cAttrs _ -> pass
      CCase cExpr cStat _ -> pass
      CCases cExpr1 cExpr2 cStat _ -> pass
      CDefault cStat _ -> pass
      CExpr mCExpr _ -> pass
      CCompound idents cBlockItems _ -> pass
      CIf cExpr cStat mCStat _ -> pass
      CSwitch cExpr cStat _ -> pass
      CWhile cExpr cStat b _ -> pass
      CFor e mcExpr1 mCExpr2 cStat _ -> pass
      CGoto ident _ -> pass
      CGotoPtr cExpr _ -> pass
      CCont _ -> pass
      CBreak _ -> pass
      CReturn (mCExpr) _ -> pass
      CAsm cAsmStmt _ -> pass




-- | GNU Assembler statement
--
-- > CAsmStatement type-qual? asm-expr out-ops in-ops clobbers _
--
-- is an inline assembler statement.
-- The only type-qualifier (if any) allowed is /volatile/.
-- @asm-expr@ is the actual assembler epxression (a string), @out-ops@ and @in-ops@ are the input
-- and output operands of the statement.
-- @clobbers@ is a list of registers which are clobbered when executing the assembler statement

-- data CAsmStmt
--   = CAsmStmt (Maybe CTypeQual)     -- maybe volatile
--                     CStrLit        -- assembler expression (String)
--                    [CAsmOperand]   -- output operands
--                    [CAsmOperand]   -- input operands
--                    [CStrLit]       -- Clobbers
--                     NodeInfo
-- 
    
    
    
    
    
-- | Assembler operand
--
-- @CAsmOperand argName? constraintExpr arg@ specifies an operand for an assembler
-- statement.

--data CAsmOperand = CAsmOperand (Maybe Ident)   -- argument name
--                                CStrLit        -- constraint expr
--                                CExpr          -- argument
--                                NodeInfo
--
                   
                   
                   
                   
-- | C99 Block items
--
--  Things that may appear in compound statements: either statements, declarations
--   or nested function definitions.

-- data CBlockItem = CBlockStmt    CStat           -- ^ A statement
--                 | CBlockDecl    CDecl           -- ^ A local declaration
--                 | CNestedFunDef CFunDef         -- ^ A nested function (GNU C)
                  
                  
                  
-- | C declaration specifiers and qualifiers
--
-- Declaration specifiers include at most one storage-class specifier (C99 6.7.1),
-- type specifiers (6.7.2) and type qualifiers (6.7.3).

--data CDeclSpec = CStorageSpec CStorageSpec  -- ^ storage-class specifier or typedef
--               | CTypeSpec    CTypeSpec     -- ^ type name
--               | CTypeQual    CTypeQual     -- ^ type qualifier


instance Dump CDeclSpec where
 dump (CStorageSpec cStorageSpec) = parens $ "CStorageSpec "++dump cStorageSpec
 dump (CTypeSpec cTypeSpec)       = parens $ "CTypeSpec "   ++dump cTypeSpec
 dump (CTypeQual cTypeQual)       = parens $ "CTypeQual "   ++dump cTypeQual



-- | C storage class specifier (and typedefs) (K&R A8.1, C99 6.7.1)

-- data CStorageSpec = CAuto     NodeInfo     -- ^ auto
--                   | CRegister NodeInfo     -- ^ register
--                   | CStatic   NodeInfo     -- ^ static
--                   | CExtern   NodeInfo     -- ^ extern
--                   | CTypedef  NodeInfo     -- ^ typedef
--                   | CThread   NodeInfo     -- ^ GNUC thread local storage

instance Dump CStorageSpec where
  dump css =
    case css of
      CAuto     _ -> "CAuto"
      CRegister _ -> "CRegister"
      CStatic   _ -> "CStatic"
      CExtern   _ -> "CExtern"
      CTypedef  _ -> "CTypedef"
      CThread   _ -> "CThread"
      
      
      
                 
                 

-- | C type specifier (K&R A8.2, C99 6.7.2)
--
-- Type specifiers are either basic types such as @char@ or @int@,
-- @struct@, @union@ or @enum@ specifiers or typedef names.
--
-- As a GNU extension, a @typeof@ expression also is a type specifier.

-- data CTypeSpec = CVoidType    NodeInfo
--                | CCharType    NodeInfo
--                | CShortType   NodeInfo
--                | CIntType     NodeInfo
--                | CLongType    NodeInfo
--                | CFloatType   NodeInfo
--                | CDoubleType  NodeInfo
--                | CSignedType  NodeInfo
--                | CUnsigType   NodeInfo
--                | CBoolType    NodeInfo
--                | CComplexType NodeInfo
--                | CSUType      CStructUnion NodeInfo  -- ^ Struct or Union specifier
--                | CEnumType    CEnum        NodeInfo  -- ^ Enumeration specifier
--                | CTypeDef     Ident        NodeInfo  -- ^ Typedef name
--                | CTypeOfExpr  CExpr        NodeInfo  -- ^ @typeof(expr)@
--                | CTypeOfType  CDecl        NodeInfo  -- ^ @typeof(type)@


instance Dump CTypeSpec where 
  dump cts = 
    case cts of
      CVoidType    _ -> "CVoidType"
      CCharType    _ -> "CCharType"
      CShortType   _ -> "CShortType"
      CIntType     _ -> "CIntType"
      CLongType    _ -> "CLongType"
      CFloatType   _ -> "CFloatType"
      CDoubleType  _ -> "CDoubleType"
      CSignedType  _ -> "CSignedType"
      CUnsigType   _ -> "CUnsigType"
      CBoolType    _ -> "CBoolType"
      CComplexType _ -> "CComplexType"
      CSUType      cStructUnion _ -> parens $ "CSUType " ++ dump cStructUnion
      CEnumType    cEnum        _ -> parens $ "CEnumType " ++ dump cEnum
      CTypeDef     ident        _ -> parens $ "CTypeDef " ++ dump ident
      CTypeOfExpr  cExpr        _ -> pass -- GNU extensions
      CTypeOfType  cDecl        _ -> pass -- 





-- | C type qualifiers (K&R A8.2, C99 6.7.3), function specifiers (C99 6.7.4), and attributes.
--
-- @const@, @volatile@ and @restrict@ type qualifiers and @inline@ function specifier.
-- Additionally, @__attribute__@ annotations for declarations and declarators.

--data CTypeQual = CConstQual NodeInfo
--               | CVolatQual NodeInfo
--               | CRestrQual NodeInfo
--               | CInlineQual NodeInfo
--               | CAttrQual  CAttr

instance Dump CTypeQual where
  dump ctq =
    case ctq of
      CConstQual  _ -> "CConstQual"
      CVolatQual  _ -> "CVolatQual"
      CRestrQual  _ -> "CRestrQual"
      CInlineQual _ -> "CInlineQual"
      CAttrQual  attr -> parens $ "CAttrQual "++ dump attr





-- | C structure or union specifiers (K&R A8.3, C99 6.7.2.1)
--
-- @CStruct tag identifier struct-decls c-attrs@ represents a struct or union specifier (depending on @tag@).
--
--   * either @identifier@ or the declaration list @struct-decls@ (or both) have to be present.
--
--     Example: in @struct foo x;@, the identifier is present, in @struct { int y; } x@ the declaration list, and
--     in @struct foo { int y; } x;@ both of them.
--
--   * @c-attrs@ is a list of @__attribute__@ annotations associated with the struct or union specifier

-- data CStructUnion = CStruct CStructTag
--                             (Maybe Ident)
--                             (Maybe [CDecl])    -- member declarations
--                             [CAttr]            -- __attribute__s
--                             NodeInfo

instance Dump CStructUnion where
  dump (CStruct cStructTag mIdent mCDecls cAttrs _) =
    parens . unwords $ ["CStructUnion",dump cStructTag,dump mIdent,dump mCDecls,dump cAttrs]
                    
                    
                    

-- | A tag to determine wheter we refer to a @struct@ or @union@, see 'CStructUnion'.

--data CStructTag = CStructTag
--                | CUnionTag

instance Dump CStructTag where
  dump CStructTag = "CStructTag" 
  dump CUnionTag  = "CUnionTag"


-- | C enumeration specifier (K&R A8.4, C99 6.7.2.2)
--
-- @CEnum identifier enumerator-list attrs@ represent as enum specifier
--
--  * Either the identifier or the enumerator-list (or both) have to be present.
--
--  * If @enumerator-list@ is present, it has to be non-empty.
--
--  * The enumerator list is of the form @(enumeration-constant, enumeration-value?)@, where the latter
--    is an optional constant integral expression.
--
--  * @attrs@ is a list of @__attribute__@ annotations associated with the enumeration specifier

--data CEnum = CEnum (Maybe Ident)
--                   (Maybe [(Ident,             -- variant name
--                            Maybe CExpr)])     -- explicit variant value
--                   [CAttr]                     -- __attribute__s
--                   NodeInfo

instance Dump CEnum where
  dump (CEnum mIdent mNameVals cAttrs _) =
    parens . unwords $ ["CEnum",dump mIdent,dump mNameVals,dump cAttrs]



-- | C initialization (K&R A8.7, C99 6.7.8)
--
-- Initializers are either assignment expressions or initializer lists
-- (surrounded in curly braces), whose elements are themselves
-- initializers, paired with an optional list of designators.

--data CInit = CInitExpr CExpr
--                       NodeInfo            -- ^ assignment expression
--           | CInitList CInitList
--                       NodeInfo            -- ^ initialization list (see 'CInitList')

instance Dump CInit where
  dump (CInitExpr cExpr _)     = parens $ "CInitExpr "++dump cExpr
  dump (CInitList cInitList _) = parens $ "CInitList "++dump cInitList




-- | Initializer List
--
-- The members of an initializer list are of the form @(designator-list,initializer)@.
-- @designator-list@ is allowed to be empty - in this case the initializer refers to the
-- ''next'' member of the compound type (see C99 6.7.8).
--
-- Examples (simplified expressions and identifiers):
--
-- > -- { [0], [3] = 4, [2] = 5, 8 }
-- > let init1 = ([CArrDesig 0, CArrDesig 3], CInitExpr 4)
-- >     init2 = ([CArrDesig 2], CInitExpr 5)
-- >     init3 = ([], CInitExpr 8)
-- > in  CInitList [init1, init2, init3]
--
-- > -- { .s = { {2,3} , .a = { 1 } } }
-- > let init_1  = [ ([], CInitExpr 1) ]
-- >     init_23 = zip (repeat []) [CInitExpr 2, CInitExpr 3]
-- >     init_s_1 = ([], CInitList init_23)
-- >     init_s_a = ([CMemberDesig "a"], CInitList init_1)
-- >     init_s  = ((CMemberDesig "s"), CInitList [init_s_1,init_s_a])
-- > in  CInitList [init_s]

--type CInitList = [([CDesignator], CInit)]





-- | Designators
--
-- A designator specifies a member of an object, either an element or range of an array,
-- or the named member of a struct \/ union.

--data CDesignator = CArrDesig     CExpr
--                                 NodeInfo  -- ^ array position designator
--                 | CMemberDesig  Ident
--                                 NodeInfo  -- ^ member designator
--                 | CRangeDesig   CExpr
--                                 CExpr
--                                 NodeInfo  -- ^ array range designator @CRangeDesig from to _@ (GNU C)

instance Dump CDesignator where
  dump _ = pass


-- | @__attribute__@ annotations
--
-- Those are of the form @CAttr attribute-name attribute-parameters@,
-- and serve as generic properties of some syntax tree elements.

--data CAttr = CAttr Ident [CExpr] NodeInfo

instance Dump CAttr where
  dump (CAttr ident cExprs _) = parens . unwords $ ["CAttr",dump ident,dumpList cExprs]



-- | C expression (K&R A7)
--
-- * these can be arbitrary expression, as the argument of `sizeof' can be
--   arbitrary, even if appearing in a constant expression
--
-- * GNU C extensions: @alignof@, @__real@, @__imag@, @({ stmt-expr })@, @&& label@ and built-ins
--

--data CExpr = CComma       [CExpr]       -- comma expression list, n >= 2
--                          NodeInfo
--           | CAssign      CAssignOp     -- assignment operator
--                          CExpr         -- l-value
--                          CExpr         -- r-value
--                          NodeInfo
--           | CCond        CExpr         -- conditional
--                   (Maybe CExpr)        -- true-expression (GNU allows omitting)
--                          CExpr         -- false-expression
--                          NodeInfo
--           | CBinary      CBinaryOp     -- binary operator
--                          CExpr         -- lhs
--                          CExpr         -- rhs
--                          NodeInfo
--           | CCast        CDecl         -- type name
--                          CExpr
--                          NodeInfo
--           | CUnary       CUnaryOp      -- unary operator
--                          CExpr
--                          NodeInfo
--           | CSizeofExpr  CExpr
--                          NodeInfo
--           | CSizeofType  CDecl         -- type name
--                          NodeInfo
--           | CAlignofExpr CExpr
--                          NodeInfo
--           | CAlignofType CDecl         -- type name
--                          NodeInfo
--           | CComplexReal CExpr         -- real part of complex number
--                          NodeInfo
--           | CComplexImag CExpr         -- imaginary part of complex number
--                          NodeInfo
--           | CIndex       CExpr         -- array
--                          CExpr         -- index
--                          NodeInfo
--           | CCall        CExpr         -- function
--                          [CExpr]       -- arguments
--                          NodeInfo
--           | CMember      CExpr         -- structure
--                          Ident         -- member name
--                          Bool          -- deref structure? (True for `->')
--                          NodeInfo
--           | CVar         Ident         -- identifier (incl. enumeration const)
--                          NodeInfo
--           | CConst       CConst        -- ^ integer, character, floating point and string constants
--           | CCompoundLit CDecl
--                          CInitList     -- type name & initialiser list
--                          NodeInfo      -- ^ C99 compound literal
--           | CStatExpr    CStat NodeInfo  -- ^ GNU C compound statement as expr
--           | CLabAddrExpr Ident NodeInfo  -- ^ GNU C address of label
--           | CBuiltinExpr CBuiltin        -- ^ builtin expressions, see 'CBuiltin'


--just enough here to get constant expressions in enum declarations
instance Dump CExpr where
  dump (CConst c) = parens $ "CConst "++ dump c
  dump (CUnary op e _) = parens . unwords $ ["CUnary",dump op,dump e]
  dump (CBinary op e1 e2 _) = parens . unwords $ ["CBinary",dump op,dump e1,dump e2]
  dump (CVar (Ident name _ _) _) = parens . unwords $ ["CVar","\""++name++"\""]
  dump _ = pass






-- | GNU Builtins, which cannot be typed in C99

--data CBuiltin =
--          CBuiltinVaArg CExpr CDecl NodeInfo            -- ^ @(expr, type)@
--        | CBuiltinOffsetOf CDecl [CDesignator] NodeInfo -- ^ @(type, designator-list)@
--        | CBuiltinTypesCompatible CDecl CDecl NodeInfo  -- ^ @(type,type)@

instance Dump CBuiltin where
  dump _ = pass


-- | C constant (K&R A2.5 & A7.2)
--data CConst = CIntConst   CInteger NodeInfo
--            | CCharConst  CChar NodeInfo
--            | CFloatConst CFloat NodeInfo
--            | CStrConst   CString NodeInfo

instance Dump CConst where
  dump (CIntConst i _)   = parens $ "CIntConst "++show i
  dump (CCharConst c _)  = parens $ "CCharConst "++show (getCChar c)
  dump (CFloatConst f _) = parens $ "CFloatConst "++show f
  dump (CStrConst s _)   = parens $ "CStrConst "++show s 


-- | Attributed string literals
--data CStrLit = CStrLit CString NodeInfo

instance Dump CStrLit where
  dump (CStrLit cString _) = show cString


-- | C binary operators (K&R A7.6-15)
--
-- data CBinaryOp = CMulOp
--                | CDivOp
--                | CRmdOp                 -- ^ remainder of division
--                | CAddOp
--                | CSubOp
--                | CShlOp                 -- ^ shift left
--                | CShrOp                 -- ^ shift right
--                | CLeOp                  -- ^ less
--                | CGrOp                  -- ^ greater
--                | CLeqOp                 -- ^ less or equal
--                | CGeqOp                 -- ^ greater or equal
--                | CEqOp                  -- ^ equal
--                | CNeqOp                 -- ^ not equal
--                | CAndOp                 -- ^ bitwise and
--                | CXorOp                 -- ^ exclusive bitwise or
--                | COrOp                  -- ^ inclusive bitwise or
--                | CLndOp                 -- ^ logical and
--                | CLorOp                 -- ^ logical or

instance Dump CBinaryOp where
  dump op =
    case op of
      CMulOp -> "CMulOp" 
      CDivOp -> "CDivOp" 
      CRmdOp -> "CRmdOp"  
      CAddOp -> "CAddOp" 
      CSubOp -> "CSubOp" 
      CShlOp -> "CShlOp"  
      CShrOp -> "CShrOp"  
      CLeOp  -> "CLeOp"  
      CGrOp  -> "CGrOp"  
      CLeqOp -> "CLeqOp"  
      CGeqOp -> "CGeqOp"  
      CEqOp  -> "CEqOp"  
      CNeqOp -> "CNeqOp"  
      CAndOp -> "CAndOp"  
      CXorOp -> "CXorOp"  
      COrOp  -> "COrOp"  
      CLndOp -> "CLndOp"  
      CLorOp -> "CLorOp"  



-- | C unary operator (K&R A7.3-4)
--
-- data CUnaryOp = CPreIncOp               -- ^ prefix increment operator
--               | CPreDecOp               -- ^ prefix decrement operator
--               | CPostIncOp              -- ^ postfix increment operator
--               | CPostDecOp              -- ^ postfix decrement operator
--               | CAdrOp                  -- ^ address operator
--               | CIndOp                  -- ^ indirection operator
--               | CPlusOp                 -- ^ prefix plus
--               | CMinOp                  -- ^ prefix minus
--               | CCompOp                 -- ^ one's complement
--               | CNegOp                  -- ^ logical negation

instance Dump CUnaryOp where
  dump op =
    case op of
      CPreIncOp  -> "CPreIncOp"
      CPreDecOp  -> "CPreDecOp"
      CPostIncOp -> "CPostIncOp"
      CPostDecOp -> "CPostDecOp"
      CAdrOp     -> "CAdrOp"   
      CIndOp     -> "CIndOp"  
      CPlusOp    -> "CPlusOp"  
      CMinOp     -> "CMinOp"   
      CCompOp    -> "CCompOp"  
      CNegOp     -> "CNegOp"   
