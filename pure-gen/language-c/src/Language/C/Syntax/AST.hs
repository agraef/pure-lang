{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
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
module Language.C.Syntax.AST (
  -- * C translation units
  CTranslUnit,  CExtDecl,
  CTranslationUnit(..),  CExternalDeclaration(..),
  -- * Declarations
  CFunDef,  CDecl, CStructUnion, CEnum,
  CFunctionDef(..),  CDeclaration(..),
  CStructTag(..), CStructureUnion(..),  CEnumeration(..),
  -- * Declaration attributes
  CDeclSpec, partitionDeclSpecs,
  CStorageSpec, CTypeSpec, isSUEDef, CTypeQual, CAttr,
  CDeclarationSpecifier(..), CStorageSpecifier(..), CTypeSpecifier(..),
  CTypeQualifier(..), CAttribute(..),
  -- * Declarators
  CDeclr,CDerivedDeclr,CArrSize,
  CDeclarator(..), CDerivedDeclarator(..), CArraySize(..),
  -- * Initialization
  CInit, CInitList, CDesignator,
  CInitializer(..), CInitializerList, CPartDesignator(..),
  -- * Statements
  CStat, CBlockItem, CAsmStmt, CAsmOperand,
  CStatement(..), CCompoundBlockItem(..),
  CAssemblyStatement(..), CAssemblyOperand(..),
  -- * Expressions
  CExpr, CExpression(..),
  CAssignOp(..), CBinaryOp(..), CUnaryOp(..),
  CBuiltin, CBuiltinThing(..),
  -- * Constants
  CConst, CStrLit, cstringOfLit, liftStrLit,
  CConstant(..), CStringLiteral(..),
  -- * Annoated type class
  Annotated(..)
) where
import Data.List
import Language.C.Syntax.Constants
import Language.C.Syntax.Ops
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position
import Data.Generics

-- | Complete C tranlsation unit (C99 6.9, K&R A10)
--
-- A complete C translation unit, for example representing a C header or source file.
-- It consists of a list of external (i.e. toplevel) declarations.
type CTranslUnit = CTranslationUnit NodeInfo
data CTranslationUnit a
  = CTranslUnit [CExternalDeclaration a] a
    deriving (Show, Data, Typeable {-! ,CNode ,Functor, Annotated !-})

-- | External C declaration (C99 6.9, K&R A10)
--
-- Either a toplevel declaration, function definition or external assembler.
type CExtDecl = CExternalDeclaration NodeInfo
data CExternalDeclaration a
  = CDeclExt (CDeclaration a)
  | CFDefExt (CFunctionDef a)
  | CAsmExt  (CStringLiteral a) a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor, Annotated !-})

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
type CFunDef = CFunctionDef NodeInfo
data CFunctionDef a
  = CFunDef
    [CDeclarationSpecifier a] -- type specifier and qualifier
    (CDeclarator a)           -- declarator
    [CDeclaration a]          -- optional declaration list
    (CStatement a)            -- compound statement
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


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
type CDecl = CDeclaration NodeInfo
data CDeclaration a
  = CDecl
    [CDeclarationSpecifier a] -- type specifier and qualifier, __attribute__
    [(Maybe (CDeclarator a),  -- declarator (may be omitted)
      Maybe (CInitializer a), -- optional initialize
      Maybe (CExpression a))] -- optional size (const expr)
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Annotated !-})

-- Derive instance is a little bit ugly
instance Functor CDeclaration where
  fmap f (CDecl specs declarators annot) =
    CDecl (map (fmap f) specs) (map fmap3m declarators) (f annot)
      where fmap3m (a,b,c) = (fmap (fmap f) a, fmap (fmap f) b, fmap (fmap f) c)

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
type CDeclr = CDeclarator NodeInfo
data CDeclarator a
  = CDeclr (Maybe Ident) [CDerivedDeclarator a] (Maybe (CStringLiteral a)) [CAttribute a] a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


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
type CDerivedDeclr = CDerivedDeclarator NodeInfo
data CDerivedDeclarator a
  = CPtrDeclr [CTypeQualifier a] a
  -- ^ Pointer declarator @CPtrDeclr tyquals declr@
  | CArrDeclr [CTypeQualifier a] (CArraySize a) a
  -- ^ Array declarator @CArrDeclr declr tyquals size-expr?@
  | CFunDeclr (Either [Ident] ([CDeclaration a],Bool)) [CAttribute a] a
    -- ^ Function declarator @CFunDeclr declr (old-style-params | new-style-params) c-attrs@
    deriving (Show, Data,Typeable {-! ,CNode , Annotated !-})

-- Derived instance relies on fmap2
instance Functor CDerivedDeclarator where
        fmap _f (CPtrDeclr a1 a2) = CPtrDeclr (fmap (fmap _f) a1) (_f a2)
        fmap _f (CArrDeclr a1 a2 a3)
          = CArrDeclr (fmap (fmap _f) a1) (fmap _f a2) (_f a3)
        fmap _f (CFunDeclr a1 a2 a3)
          = CFunDeclr (fmap (fmapFirst (fmap (fmap _f))) a1) (fmap (fmap _f) a2)
              (_f a3)
          where fmapFirst f (a,b) = (f a, b)

-- | Size of an array
type CArrSize = CArraySize NodeInfo
data CArraySize a
  = CNoArrSize Bool               -- ^ @CUnknownSize isCompleteType@
  | CArrSize Bool (CExpression a) -- ^ @CArrSize isStatic expr@
    deriving (Show, Data,Typeable {-! , Functor !-})


-- | C statement (K&R A9, C99 6.8)
--
type CStat = CStatement NodeInfo
data CStatement a
  -- | An (attributed) label followed by a statement
  = CLabel  Ident (CStatement a) [CAttribute a] a
  -- | A statement of the form @case expr : stmt@
  | CCase (CExpression a) (CStatement a) a
  -- | A case range of the form @case lower ... upper : stmt@
  | CCases (CExpression a) (CExpression a) (CStatement a) a
  -- | The default case @default : stmt@
  | CDefault (CStatement a) a
  -- | A simple statement, that is in C: evaluating an expression with
  --   side-effects and discarding the result.
  | CExpr (Maybe (CExpression a)) a
  -- | compound statement @CCompound localLabels blockItems at@
  | CCompound [Ident] [CCompoundBlockItem a] a
  -- | conditional statement @CIf ifExpr thenStmt maybeElseStmt at@
  | CIf (CExpression a) (CStatement a) (Maybe (CStatement a)) a
  -- | switch statement @CSwitch selectorExpr switchStmt@, where
  -- @switchStmt@ usually includes /case/, /break/ and /default/
  -- statements
  | CSwitch (CExpression a) (CStatement a) a
  -- | while or do-while statement @CWhile guard stmt isDoWhile at@
  | CWhile (CExpression a) (CStatement a) Bool a
  -- | for statement @CFor init expr-2 expr-3 stmt@, where @init@ is
  -- either a declaration or initializing expression
  | CFor (Either (Maybe (CExpression a)) (CDeclaration a))
    (Maybe (CExpression a))
    (Maybe (CExpression a))
    (CStatement a)
    a
  -- | goto statement @CGoto label@
  | CGoto Ident a
  -- | computed goto @CGotoPtr labelExpr@
  | CGotoPtr (CExpression a) a
  -- | continue statement
  | CCont a
  -- | break statement
  | CBreak a
  -- | return statement @CReturn returnExpr@
  | CReturn (Maybe (CExpression a)) a
  -- | assembly statement
  | CAsm (CAssemblyStatement a) a
    deriving (Show, Data,Typeable {-! , CNode , Annotated !-})

-- Derived instance relies on fmap2 :(
instance Functor CStatement where
        fmap _f (CLabel a1 a2 a3 a4)
          = CLabel a1 (fmap _f a2) (fmap (fmap _f) a3) (_f a4)
        fmap _f (CCase a1 a2 a3) = CCase (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CCases a1 a2 a3 a4)
          = CCases (fmap _f a1) (fmap _f a2) (fmap _f a3) (_f a4)
        fmap _f (CDefault a1 a2) = CDefault (fmap _f a1) (_f a2)
        fmap _f (CExpr a1 a2) = CExpr (fmap (fmap _f) a1) (_f a2)
        fmap _f (CCompound a1 a2 a3)
          = CCompound a1 (fmap (fmap _f) a2) (_f a3)
        fmap _f (CIf a1 a2 a3 a4)
          = CIf (fmap _f a1) (fmap _f a2) (fmap (fmap _f) a3) (_f a4)
        fmap _f (CSwitch a1 a2 a3)
          = CSwitch (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CWhile a1 a2 a3 a4)
          = CWhile (fmap _f a1) (fmap _f a2) a3 (_f a4)
        fmap _f (CFor a1 a2 a3 a4 a5)
          = CFor (mapEither (fmap (fmap _f)) (fmap _f) a1)
                 (fmap (fmap _f) a2) (fmap (fmap _f) a3) (fmap _f a4)
                 (_f a5)
          where mapEither f1 f2 = either (Left . f1) (Right . f2)
        fmap _f (CGoto a1 a2) = CGoto a1 (_f a2)
        fmap _f (CGotoPtr a1 a2) = CGotoPtr (fmap _f a1) (_f a2)
        fmap _f (CCont a1) = CCont (_f a1)
        fmap _f (CBreak a1) = CBreak (_f a1)
        fmap _f (CReturn a1 a2) = CReturn (fmap (fmap _f) a1) (_f a2)
        fmap _f (CAsm a1 a2) = CAsm (fmap _f a1) (_f a2)

-- | GNU Assembler statement
--
-- > CAssemblyStatement type-qual? asm-expr out-ops in-ops clobbers _
--
-- is an inline assembler statement.
-- The only type-qualifier (if any) allowed is /volatile/.
-- @asm-expr@ is the actual assembler epxression (a string), @out-ops@ and @in-ops@ are the input
-- and output operands of the statement.
-- @clobbers@ is a list of registers which are clobbered when executing the assembler statement
type CAsmStmt = CAssemblyStatement NodeInfo
data CAssemblyStatement a
  = CAsmStmt
    (Maybe (CTypeQualifier a)) -- maybe volatile
    (CStringLiteral a)         -- assembler expression (String)
    [CAssemblyOperand a]       -- output operands
    [CAssemblyOperand a]       -- input operands
    [CStringLiteral a]         -- Clobbers
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})

-- | Assembler operand
--
-- @CAsmOperand argName? constraintExpr arg@ specifies an operand for an assembler
-- statement.
type CAsmOperand = CAssemblyOperand NodeInfo
data CAssemblyOperand a
  = CAsmOperand
    (Maybe Ident)       -- argument name
    (CStringLiteral a)  -- constraint expr
    (CExpression a)     -- argument
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})

-- | C99 Block items
--
--  Things that may appear in compound statements: either statements, declarations
--   or nested function definitions.
type CBlockItem = CCompoundBlockItem NodeInfo
data CCompoundBlockItem a
  = CBlockStmt    (CStatement a)    -- ^ A statement
  | CBlockDecl    (CDeclaration a)  -- ^ A local declaration
  | CNestedFunDef (CFunctionDef a)  -- ^ A nested function (GNU C)
    deriving (Show, Data,Typeable {-! , CNode , Functor, Annotated !-})

-- | C declaration specifiers and qualifiers
--
-- Declaration specifiers include at most one storage-class specifier (C99 6.7.1),
-- type specifiers (6.7.2) and type qualifiers (6.7.3).
type CDeclSpec = CDeclarationSpecifier NodeInfo
data CDeclarationSpecifier a
  = CStorageSpec (CStorageSpecifier a) -- ^ storage-class specifier or typedef
  | CTypeSpec    (CTypeSpecifier a)    -- ^ type name
  | CTypeQual    (CTypeQualifier a)    -- ^ type qualifier
    deriving (Show, Data,Typeable {-! ,CNode ,Functor, Annotated !-})


-- | Separate the declaration specifiers
--
-- Note that inline isn't actually a type qualifier, but a function specifier.
-- @__attribute__@ of a declaration qualify declarations or declarators (but not types),
-- and are therefore separated as well.
partitionDeclSpecs :: [CDeclarationSpecifier a]
                   -> ( [CStorageSpecifier a], [CAttribute a]
                      , [CTypeQualifier a], [CTypeSpecifier a], Bool)
partitionDeclSpecs = foldr deals ([],[],[],[],False) where
    deals (CTypeQual (CInlineQual _)) (sts,ats,tqs,tss,_) = (sts,ats,tqs,tss,True)
    deals (CStorageSpec sp) (sts,ats,tqs,tss,inline)  = (sp:sts,ats,tqs,tss,inline)
    deals (CTypeQual (CAttrQual attr)) (sts,ats,tqs,tss,inline)  = (sts,attr:ats,tqs,tss,inline)
    deals (CTypeQual tq) (sts,ats,tqs,tss,inline)     = (sts,ats,tq:tqs,tss,inline)
    deals (CTypeSpec ts) (sts,ats,tqs,tss,inline)     = (sts,ats,tqs,ts:tss,inline)

-- | C storage class specifier (and typedefs) (K&R A8.1, C99 6.7.1)
type CStorageSpec = CStorageSpecifier NodeInfo
data CStorageSpecifier a
  = CAuto     a     -- ^ auto
  | CRegister a     -- ^ register
  | CStatic   a     -- ^ static
  | CExtern   a     -- ^ extern
  | CTypedef  a     -- ^ typedef
  | CThread   a     -- ^ GNUC thread local storage
    deriving (Show, Eq,Ord,Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | C type specifier (K&R A8.2, C99 6.7.2)
--
-- Type specifiers are either basic types such as @char@ or @int@,
-- @struct@, @union@ or @enum@ specifiers or typedef names.
--
-- As a GNU extension, a @typeof@ expression also is a type specifier.
type CTypeSpec = CTypeSpecifier NodeInfo
data CTypeSpecifier a
  = CVoidType    a
  | CCharType    a
  | CShortType   a
  | CIntType     a
  | CLongType    a
  | CFloatType   a
  | CFloat128Type a
  | CDoubleType  a
  | CSignedType  a
  | CUnsigType   a
  | CBoolType    a
  | CComplexType a
  | CInt128Type  a
  | CSUType      (CStructureUnion a) a      -- ^ Struct or Union specifier
  | CEnumType    (CEnumeration a)    a      -- ^ Enumeration specifier
  | CTypeDef     Ident        a      -- ^ Typedef name
  | CTypeOfExpr  (CExpression a)  a  -- ^ @typeof(expr)@
  | CTypeOfType  (CDeclaration a) a  -- ^ @typeof(type)@
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | returns @True@ if the given typespec is a struct, union or enum /definition/
isSUEDef :: CTypeSpecifier a -> Bool
isSUEDef (CSUType (CStruct _ _ (Just _) _ _) _) = True
isSUEDef (CEnumType (CEnum _ (Just _) _ _) _) = True
isSUEDef _ = False

-- | C type qualifiers (K&R A8.2, C99 6.7.3), function specifiers (C99 6.7.4), and attributes.
--
-- @const@, @volatile@ and @restrict@ type qualifiers and @inline@ function specifier.
-- Additionally, @__attribute__@ annotations for declarations and declarators.
type CTypeQual = CTypeQualifier NodeInfo
data CTypeQualifier a
  = CConstQual a
  | CVolatQual a
  | CRestrQual a
  | CInlineQual a
  | CAttrQual  (CAttribute a)
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


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
type CStructUnion = CStructureUnion NodeInfo
data CStructureUnion a
  = CStruct
    CStructTag
    (Maybe Ident)
    (Maybe [CDeclaration a])  -- member declarations
    [CAttribute a]            -- __attribute__s
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | A tag to determine wheter we refer to a @struct@ or @union@, see 'CStructUnion'.
data CStructTag = CStructTag
                | CUnionTag
                deriving (Show, Eq,Data,Typeable)


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
type CEnum = CEnumeration NodeInfo
data CEnumeration a
  = CEnum
    (Maybe Ident)
    (Maybe [(Ident,                   -- variant name
             Maybe (CExpression a))]) -- explicit variant value
    [CAttribute a]                    -- __attribute__s
    a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})



-- | C initialization (K&R A8.7, C99 6.7.8)
--
-- Initializers are either assignment expressions or initializer lists
-- (surrounded in curly braces), whose elements are themselves
-- initializers, paired with an optional list of designators.
type CInit = CInitializer NodeInfo
data CInitializer a
  -- | assignment expression
  = CInitExpr (CExpression a) a
  -- | initialization list (see 'CInitList')
  | CInitList (CInitializerList a) a
    deriving (Show, Data,Typeable {-! ,CNode , Annotated !-})

-- deriving Functor does not work (type synonym)
instance Functor CInitializer where
        fmap _f (CInitExpr a1 a2) = CInitExpr (fmap _f a1) (_f a2)
        fmap _f (CInitList a1 a2) = CInitList (fmapInitList _f a1) (_f a2)
fmapInitList :: (a->b) -> (CInitializerList a) -> (CInitializerList b)
fmapInitList _f = map (\(desigs, initializer) -> (fmap (fmap _f) desigs, fmap _f initializer))

-- | Initializer List
--
-- The members of an initializer list are of the form @(designator-list,initializer)@.
-- The @designator-list@ specifies one member of the compound type which is initialized.
-- It is allowed to be empty - in this case the initializer refers to the
-- ''next'' member of the compound type (see C99 6.7.8).
--
-- Examples (simplified expressions and identifiers):
--
-- > -- int x[3][4] = { [0][3] = 4, [2] = 5, 8 };
-- > --   corresponds to the assignments
-- > -- x[0][3] = 4; x[2][0] = 5; x[2][1] = 8;
-- > let init1 = ([CArrDesig 0, CArrDesig 3], CInitExpr 4)
-- >     init2 = ([CArrDesig 2]             , CInitExpr 5)
-- >     init3 = ([]                        , CInitExpr 8)
-- > in  CInitList [init1, init2, init3]
--
-- > -- struct { struct { int a[2]; int b[2]; int c[2]; } s; } x = { .s = { {2,3} , .c[0] = 1 } };
-- > --   corresponds to the assignments
-- > -- x.s.a[0] = 2; x.s.a[1] = 3; x.s.c[0] = 1;
-- > let init_s_0 = CInitList [ ([], CInitExpr 2), ([], CInitExpr 3)]
-- >     init_s   = CInitList [
-- >                            ([], init_s_0),
-- >                            ([CMemberDesig "c", CArrDesig 0], CInitExpr 1)
-- >                          ]
-- > in  CInitList [(CMemberDesig "s", init_s)]
type CInitList = CInitializerList NodeInfo
type CInitializerList a = [([CPartDesignator a], CInitializer a)]

-- | Designators
--
-- A designator specifies a member of an object, either an element or range of an array,
-- or the named member of a struct \/ union.
type CDesignator = CPartDesignator NodeInfo
data CPartDesignator a
  -- | array position designator
  = CArrDesig     (CExpression a) a
  -- | member designator
  | CMemberDesig  Ident a
  -- | array range designator @CRangeDesig from to _@ (GNU C)
  | CRangeDesig (CExpression a) (CExpression a) a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | @__attribute__@ annotations
--
-- Those are of the form @CAttr attribute-name attribute-parameters@,
-- and serve as generic properties of some syntax tree elements.
type CAttr = CAttribute NodeInfo
data CAttribute a = CAttr Ident [CExpression a] a
                    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | C expression (K&R A7)
--
-- * these can be arbitrary expression, as the argument of `sizeof' can be
--   arbitrary, even if appearing in a constant expression
--
-- * GNU C extensions: @alignof@, @__real@, @__imag@, @({ stmt-expr })@, @&& label@ and built-ins
--
type CExpr = CExpression NodeInfo
data CExpression a
  = CComma       [CExpression a]         -- comma expression list, n >= 2
                 a
  | CAssign      CAssignOp               -- assignment operator
                 (CExpression a)         -- l-value
                 (CExpression a)         -- r-value
                 a
  | CCond        (CExpression a)         -- conditional
                 (Maybe (CExpression a)) -- true-expression (GNU allows omitting)
                 (CExpression a)         -- false-expression
                 a
  | CBinary      CBinaryOp               -- binary operator
                 (CExpression a)         -- lhs
                 (CExpression a)         -- rhs
                 a
  | CCast        (CDeclaration a)        -- type name
                 (CExpression a)
                 a
  | CUnary       CUnaryOp                -- unary operator
                 (CExpression a)
                 a
  | CSizeofExpr  (CExpression a)
                 a
  | CSizeofType  (CDeclaration a)        -- type name
                 a
  | CAlignofExpr (CExpression a)
                 a
  | CAlignofType (CDeclaration a)        -- type name
                 a
  | CComplexReal (CExpression a)         -- real part of complex number
                 a
  | CComplexImag (CExpression a)         -- imaginary part of complex number
                 a
  | CIndex       (CExpression a)         -- array
                 (CExpression a)         -- index
                 a
  | CCall        (CExpression a)         -- function
                 [CExpression a]         -- arguments
                 a
  | CMember      (CExpression a)         -- structure
                 Ident                   -- member name
                 Bool                    -- deref structure? (True for `->')
                 a
  | CVar         Ident                   -- identifier (incl. enumeration const)
                 a
  | CConst       (CConstant a)           -- ^ integer, character, floating point and string constants
  | CCompoundLit (CDeclaration a)
                 (CInitializerList a)    -- type name & initialiser list
                 a                       -- ^ C99 compound literal
  | CStatExpr    (CStatement a) a        -- ^ GNU C compound statement as expr
  | CLabAddrExpr Ident a                 -- ^ GNU C address of label
  | CBuiltinExpr (CBuiltinThing a)       -- ^ builtin expressions, see 'CBuiltin'
    deriving (Data,Typeable,Show {-! ,CNode , Annotated !-})

-- deriving Functor does not work (type synonyms)
instance Functor CExpression where
        fmap _f (CComma a1 a2) = CComma (fmap (fmap _f) a1) (_f a2)
        fmap _f (CAssign a1 a2 a3 a4)
          = CAssign a1 (fmap _f a2) (fmap _f a3) (_f a4)
        fmap _f (CCond a1 a2 a3 a4)
          = CCond (fmap _f a1) (fmap (fmap _f) a2) (fmap _f a3) (_f a4)
        fmap _f (CBinary a1 a2 a3 a4)
          = CBinary a1 (fmap _f a2) (fmap _f a3) (_f a4)
        fmap _f (CCast a1 a2 a3) = CCast (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CUnary a1 a2 a3) = CUnary a1 (fmap _f a2) (_f a3)
        fmap _f (CSizeofExpr a1 a2) = CSizeofExpr (fmap _f a1) (_f a2)
        fmap _f (CSizeofType a1 a2) = CSizeofType (fmap _f a1) (_f a2)
        fmap _f (CAlignofExpr a1 a2) = CAlignofExpr (fmap _f a1) (_f a2)
        fmap _f (CAlignofType a1 a2) = CAlignofType (fmap _f a1) (_f a2)
        fmap _f (CComplexReal a1 a2) = CComplexReal (fmap _f a1) (_f a2)
        fmap _f (CComplexImag a1 a2) = CComplexImag (fmap _f a1) (_f a2)
        fmap _f (CIndex a1 a2 a3)
          = CIndex (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CCall a1 a2 a3)
          = CCall (fmap _f a1) (fmap (fmap _f) a2) (_f a3)
        fmap _f (CMember a1 a2 a3 a4) = CMember (fmap _f a1) a2 a3 (_f a4)
        fmap _f (CVar a1 a2) = CVar a1 (_f a2)
        fmap _f (CConst a1) = CConst (fmap _f a1)
        fmap _f (CCompoundLit a1 a2 a3)
          = CCompoundLit (fmap _f a1) (fmapInitList _f a2) (_f a3)
        fmap _f (CStatExpr a1 a2) = CStatExpr (fmap _f a1) (_f a2)
        fmap _f (CLabAddrExpr a1 a2) = CLabAddrExpr a1 (_f a2)
        fmap _f (CBuiltinExpr a1) = CBuiltinExpr (fmap _f a1)


-- | GNU Builtins, which cannot be typed in C99
type CBuiltin = CBuiltinThing NodeInfo
data CBuiltinThing a
  = CBuiltinVaArg (CExpression a) (CDeclaration a) a            -- ^ @(expr, type)@
  | CBuiltinOffsetOf (CDeclaration a) [CPartDesignator a] a -- ^ @(type, designator-list)@
  | CBuiltinTypesCompatible (CDeclaration a) (CDeclaration a) a  -- ^ @(type,type)@
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | C constant (K&R A2.5 & A7.2)
type CConst = CConstant NodeInfo
data CConstant a
  = CIntConst   CInteger a
  | CCharConst  CChar a
  | CFloatConst CFloat a
  | CStrConst   CString a
    deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | Attributed string literals
type CStrLit = CStringLiteral NodeInfo
data CStringLiteral a = CStrLit CString a
            deriving (Show, Data,Typeable {-! ,CNode ,Functor ,Annotated !-})


cstringOfLit :: CStringLiteral a -> CString
cstringOfLit (CStrLit cstr _) = cstr

-- | Lift a string literal to a C constant
liftStrLit :: CStringLiteral a -> CConstant a
liftStrLit (CStrLit str at) = CStrConst str at

-- | All AST nodes are annotated. Inspired by the Annotated
-- class of Niklas Broberg's haskell-src-exts package.
-- In principle, we could have Copointed superclass instead
-- of @ann@, for the price of another dependency.
class (Functor ast) => Annotated ast where
  -- | get the annotation of an AST node
  annotation :: ast a -> a
  -- | change the annotation (non-recursively)
  --   of an AST node. Use fmap for recursively
  --   modifying the annotation.
  amap  :: (a->a) -> ast a -> ast a

-- fmap2 :: (a->a') -> (a,b) -> (a',b)
-- fmap2 f (a,b) = (f a, b)

-- Instances generated using derive-2.*
-- GENERATED START


instance (CNode t1) => CNode (CTranslationUnit t1) where
        nodeInfo (CTranslUnit _ n) = nodeInfo n

instance (CNode t1) => Pos (CTranslationUnit t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CTranslationUnit where
        fmap _f (CTranslUnit a1 a2)
          = CTranslUnit (fmap (fmap _f) a1) (_f a2)


instance Annotated CTranslationUnit where
        annotation (CTranslUnit _ n) = n
        amap f (CTranslUnit a_1 a_2) = CTranslUnit a_1 (f a_2)


instance (CNode t1) => CNode (CExternalDeclaration t1) where
        nodeInfo (CDeclExt d) = nodeInfo d
        nodeInfo (CFDefExt d) = nodeInfo d
        nodeInfo (CAsmExt _ n) = nodeInfo n

instance (CNode t1) => Pos (CExternalDeclaration t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CExternalDeclaration where
        fmap _f (CDeclExt a1) = CDeclExt (fmap _f a1)
        fmap _f (CFDefExt a1) = CFDefExt (fmap _f a1)
        fmap _f (CAsmExt a1 a2) = CAsmExt (fmap _f a1) (_f a2)


instance Annotated CExternalDeclaration where
        annotation (CDeclExt n) = annotation n
        annotation (CFDefExt n) = annotation n
        annotation (CAsmExt _ n) = n
        amap f (CDeclExt n) = CDeclExt (amap f n)
        amap f (CFDefExt n) = CFDefExt (amap f n)
        amap f (CAsmExt a_1 a_2) = CAsmExt a_1 (f a_2)


instance (CNode t1) => CNode (CFunctionDef t1) where
        nodeInfo (CFunDef _ _ _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CFunctionDef t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CFunctionDef where
        fmap _f (CFunDef a1 a2 a3 a4 a5)
          = CFunDef (fmap (fmap _f) a1) (fmap _f a2) (fmap (fmap _f) a3)
              (fmap _f a4)
              (_f a5)


instance Annotated CFunctionDef where
        annotation (CFunDef _ _ _ _ n) = n
        amap f (CFunDef a_1 a_2 a_3 a_4 a_5)
          = CFunDef a_1 a_2 a_3 a_4 (f a_5)


instance (CNode t1) => CNode (CDeclaration t1) where
        nodeInfo (CDecl _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CDeclaration t1) where
        posOf x = posOf (nodeInfo x)


instance Annotated CDeclaration where
        annotation (CDecl _ _ n) = n
        amap f (CDecl a_1 a_2 a_3) = CDecl a_1 a_2 (f a_3)


instance (CNode t1) => CNode (CDeclarator t1) where
        nodeInfo (CDeclr _ _ _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CDeclarator t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CDeclarator where
        fmap _f (CDeclr a1 a2 a3 a4 a5)
          = CDeclr a1 (fmap (fmap _f) a2) (fmap (fmap _f) a3)
              (fmap (fmap _f) a4)
              (_f a5)


instance Annotated CDeclarator where
        annotation (CDeclr _ _ _ _ n) = n
        amap f (CDeclr a_1 a_2 a_3 a_4 a_5)
          = CDeclr a_1 a_2 a_3 a_4 (f a_5)


instance (CNode t1) => CNode (CDerivedDeclarator t1) where
        nodeInfo (CPtrDeclr _ n) = nodeInfo n
        nodeInfo (CArrDeclr _ _ n) = nodeInfo n
        nodeInfo (CFunDeclr _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CDerivedDeclarator t1) where
        posOf x = posOf (nodeInfo x)


instance Annotated CDerivedDeclarator where
        annotation (CPtrDeclr _ n) = n
        annotation (CArrDeclr _ _ n) = n
        annotation (CFunDeclr _ _ n) = n
        amap f (CPtrDeclr a_1 a_2) = CPtrDeclr a_1 (f a_2)
        amap f (CArrDeclr a_1 a_2 a_3) = CArrDeclr a_1 a_2 (f a_3)
        amap f (CFunDeclr a_1 a_2 a_3) = CFunDeclr a_1 a_2 (f a_3)


instance Functor CArraySize where
        fmap _ (CNoArrSize a1) = CNoArrSize a1
        fmap _f (CArrSize a1 a2) = CArrSize a1 (fmap _f a2)


instance (CNode t1) => CNode (CStatement t1) where
        nodeInfo (CLabel _ _ _ n) = nodeInfo n
        nodeInfo (CCase _ _ n) = nodeInfo n
        nodeInfo (CCases _ _ _ n) = nodeInfo n
        nodeInfo (CDefault _ n) = nodeInfo n
        nodeInfo (CExpr _ n) = nodeInfo n
        nodeInfo (CCompound _ _ n) = nodeInfo n
        nodeInfo (CIf _ _ _ n) = nodeInfo n
        nodeInfo (CSwitch _ _ n) = nodeInfo n
        nodeInfo (CWhile _ _ _ n) = nodeInfo n
        nodeInfo (CFor _ _ _ _ n) = nodeInfo n
        nodeInfo (CGoto _ n) = nodeInfo n
        nodeInfo (CGotoPtr _ n) = nodeInfo n
        nodeInfo (CCont d) = nodeInfo d
        nodeInfo (CBreak d) = nodeInfo d
        nodeInfo (CReturn _ n) = nodeInfo n
        nodeInfo (CAsm _ n) = nodeInfo n

instance (CNode t1) => Pos (CStatement t1) where
        posOf x = posOf (nodeInfo x)


instance Annotated CStatement where
        annotation (CLabel _ _ _ n) = n
        annotation (CCase _ _ n) = n
        annotation (CCases _ _ _ n) = n
        annotation (CDefault _ n) = n
        annotation (CExpr _ n) = n
        annotation (CCompound _ _ n) = n
        annotation (CIf _ _ _ n) = n
        annotation (CSwitch _ _ n) = n
        annotation (CWhile _ _ _ n) = n
        annotation (CFor _ _ _ _ n) = n
        annotation (CGoto _ n) = n
        annotation (CGotoPtr _ n) = n
        annotation (CCont n) = n
        annotation (CBreak n) = n
        annotation (CReturn _ n) = n
        annotation (CAsm _ n) = n
        amap f (CLabel a_1 a_2 a_3 a_4) = CLabel a_1 a_2 a_3 (f a_4)
        amap f (CCase a_1 a_2 a_3) = CCase a_1 a_2 (f a_3)
        amap f (CCases a_1 a_2 a_3 a_4) = CCases a_1 a_2 a_3 (f a_4)
        amap f (CDefault a_1 a_2) = CDefault a_1 (f a_2)
        amap f (CExpr a_1 a_2) = CExpr a_1 (f a_2)
        amap f (CCompound a_1 a_2 a_3) = CCompound a_1 a_2 (f a_3)
        amap f (CIf a_1 a_2 a_3 a_4) = CIf a_1 a_2 a_3 (f a_4)
        amap f (CSwitch a_1 a_2 a_3) = CSwitch a_1 a_2 (f a_3)
        amap f (CWhile a_1 a_2 a_3 a_4) = CWhile a_1 a_2 a_3 (f a_4)
        amap f (CFor a_1 a_2 a_3 a_4 a_5) = CFor a_1 a_2 a_3 a_4 (f a_5)
        amap f (CGoto a_1 a_2) = CGoto a_1 (f a_2)
        amap f (CGotoPtr a_1 a_2) = CGotoPtr a_1 (f a_2)
        amap f (CCont a_1) = CCont (f a_1)
        amap f (CBreak a_1) = CBreak (f a_1)
        amap f (CReturn a_1 a_2) = CReturn a_1 (f a_2)
        amap f (CAsm a_1 a_2) = CAsm a_1 (f a_2)


instance (CNode t1) => CNode (CAssemblyStatement t1) where
        nodeInfo (CAsmStmt _ _ _ _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CAssemblyStatement t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CAssemblyStatement where
        fmap _f (CAsmStmt a1 a2 a3 a4 a5 a6)
          = CAsmStmt (fmap (fmap _f) a1) (fmap _f a2) (fmap (fmap _f) a3)
              (fmap (fmap _f) a4)
              (fmap (fmap _f) a5)
              (_f a6)


instance Annotated CAssemblyStatement where
        annotation (CAsmStmt _ _ _ _ _ n) = n
        amap f (CAsmStmt a_1 a_2 a_3 a_4 a_5 a_6)
          = CAsmStmt a_1 a_2 a_3 a_4 a_5 (f a_6)


instance (CNode t1) => CNode (CAssemblyOperand t1) where
        nodeInfo (CAsmOperand _ _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CAssemblyOperand t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CAssemblyOperand where
        fmap _f (CAsmOperand a1 a2 a3 a4)
          = CAsmOperand a1 (fmap _f a2) (fmap _f a3) (_f a4)


instance Annotated CAssemblyOperand where
        annotation (CAsmOperand _ _ _ n) = n
        amap f (CAsmOperand a_1 a_2 a_3 a_4)
          = CAsmOperand a_1 a_2 a_3 (f a_4)


instance (CNode t1) => CNode (CCompoundBlockItem t1) where
        nodeInfo (CBlockStmt d) = nodeInfo d
        nodeInfo (CBlockDecl d) = nodeInfo d
        nodeInfo (CNestedFunDef d) = nodeInfo d

instance (CNode t1) => Pos (CCompoundBlockItem t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CCompoundBlockItem where
        fmap _f (CBlockStmt a1) = CBlockStmt (fmap _f a1)
        fmap _f (CBlockDecl a1) = CBlockDecl (fmap _f a1)
        fmap _f (CNestedFunDef a1) = CNestedFunDef (fmap _f a1)


instance Annotated CCompoundBlockItem where
        annotation (CBlockStmt n) = annotation n
        annotation (CBlockDecl n) = annotation n
        annotation (CNestedFunDef n) = annotation n
        amap f (CBlockStmt n) = CBlockStmt (amap f n)
        amap f (CBlockDecl n) = CBlockDecl (amap f n)
        amap f (CNestedFunDef n) = CNestedFunDef (amap f n)


instance (CNode t1) => CNode (CDeclarationSpecifier t1) where
        nodeInfo (CStorageSpec d) = nodeInfo d
        nodeInfo (CTypeSpec d) = nodeInfo d
        nodeInfo (CTypeQual d) = nodeInfo d

instance (CNode t1) => Pos (CDeclarationSpecifier t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CDeclarationSpecifier where
        fmap _f (CStorageSpec a1) = CStorageSpec (fmap _f a1)
        fmap _f (CTypeSpec a1) = CTypeSpec (fmap _f a1)
        fmap _f (CTypeQual a1) = CTypeQual (fmap _f a1)


instance Annotated CDeclarationSpecifier where
        annotation (CStorageSpec n) = annotation n
        annotation (CTypeSpec n) = annotation n
        annotation (CTypeQual n) = annotation n
        amap f (CStorageSpec n) = CStorageSpec (amap f n)
        amap f (CTypeSpec n) = CTypeSpec (amap f n)
        amap f (CTypeQual n) = CTypeQual (amap f n)


instance (CNode t1) => CNode (CStorageSpecifier t1) where
        nodeInfo (CAuto d) = nodeInfo d
        nodeInfo (CRegister d) = nodeInfo d
        nodeInfo (CStatic d) = nodeInfo d
        nodeInfo (CExtern d) = nodeInfo d
        nodeInfo (CTypedef d) = nodeInfo d
        nodeInfo (CThread d) = nodeInfo d

instance (CNode t1) => Pos (CStorageSpecifier t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CStorageSpecifier where
        fmap _f (CAuto a1) = CAuto (_f a1)
        fmap _f (CRegister a1) = CRegister (_f a1)
        fmap _f (CStatic a1) = CStatic (_f a1)
        fmap _f (CExtern a1) = CExtern (_f a1)
        fmap _f (CTypedef a1) = CTypedef (_f a1)
        fmap _f (CThread a1) = CThread (_f a1)


instance Annotated CStorageSpecifier where
        annotation (CAuto n) = n
        annotation (CRegister n) = n
        annotation (CStatic n) = n
        annotation (CExtern n) = n
        annotation (CTypedef n) = n
        annotation (CThread n) = n
        amap f (CAuto a_1) = CAuto (f a_1)
        amap f (CRegister a_1) = CRegister (f a_1)
        amap f (CStatic a_1) = CStatic (f a_1)
        amap f (CExtern a_1) = CExtern (f a_1)
        amap f (CTypedef a_1) = CTypedef (f a_1)
        amap f (CThread a_1) = CThread (f a_1)


instance (CNode t1) => CNode (CTypeSpecifier t1) where
        nodeInfo (CVoidType d) = nodeInfo d
        nodeInfo (CCharType d) = nodeInfo d
        nodeInfo (CShortType d) = nodeInfo d
        nodeInfo (CIntType d) = nodeInfo d
        nodeInfo (CLongType d) = nodeInfo d
        nodeInfo (CFloatType d) = nodeInfo d
        nodeInfo (CFloat128Type d) = nodeInfo d
        nodeInfo (CDoubleType d) = nodeInfo d
        nodeInfo (CSignedType d) = nodeInfo d
        nodeInfo (CUnsigType d) = nodeInfo d
        nodeInfo (CBoolType d) = nodeInfo d
        nodeInfo (CComplexType d) = nodeInfo d
        nodeInfo (CInt128Type d) = nodeInfo d
        nodeInfo (CSUType _ n) = nodeInfo n
        nodeInfo (CEnumType _ n) = nodeInfo n
        nodeInfo (CTypeDef _ n) = nodeInfo n
        nodeInfo (CTypeOfExpr _ n) = nodeInfo n
        nodeInfo (CTypeOfType _ n) = nodeInfo n

instance (CNode t1) => Pos (CTypeSpecifier t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CTypeSpecifier where
        fmap _f (CVoidType a1) = CVoidType (_f a1)
        fmap _f (CCharType a1) = CCharType (_f a1)
        fmap _f (CShortType a1) = CShortType (_f a1)
        fmap _f (CIntType a1) = CIntType (_f a1)
        fmap _f (CLongType a1) = CLongType (_f a1)
        fmap _f (CFloatType a1) = CFloatType (_f a1)
        fmap _f (CFloat128Type a1) = CFloat128Type (_f a1)
        fmap _f (CDoubleType a1) = CDoubleType (_f a1)
        fmap _f (CSignedType a1) = CSignedType (_f a1)
        fmap _f (CUnsigType a1) = CUnsigType (_f a1)
        fmap _f (CBoolType a1) = CBoolType (_f a1)
        fmap _f (CComplexType a1) = CComplexType (_f a1)
        fmap _f (CInt128Type a1) = CInt128Type (_f a1)
        fmap _f (CSUType a1 a2) = CSUType (fmap _f a1) (_f a2)
        fmap _f (CEnumType a1 a2) = CEnumType (fmap _f a1) (_f a2)
        fmap _f (CTypeDef a1 a2) = CTypeDef a1 (_f a2)
        fmap _f (CTypeOfExpr a1 a2) = CTypeOfExpr (fmap _f a1) (_f a2)
        fmap _f (CTypeOfType a1 a2) = CTypeOfType (fmap _f a1) (_f a2)


instance Annotated CTypeSpecifier where
        annotation (CVoidType n) = n
        annotation (CCharType n) = n
        annotation (CShortType n) = n
        annotation (CIntType n) = n
        annotation (CLongType n) = n
        annotation (CFloatType n) = n
        annotation (CFloat128Type n) = n
        annotation (CDoubleType n) = n
        annotation (CSignedType n) = n
        annotation (CUnsigType n) = n
        annotation (CBoolType n) = n
        annotation (CComplexType n) = n
        annotation (CInt128Type n) = n
        annotation (CSUType _ n) = n
        annotation (CEnumType _ n) = n
        annotation (CTypeDef _ n) = n
        annotation (CTypeOfExpr _ n) = n
        annotation (CTypeOfType _ n) = n
        amap f (CVoidType a_1) = CVoidType (f a_1)
        amap f (CCharType a_1) = CCharType (f a_1)
        amap f (CShortType a_1) = CShortType (f a_1)
        amap f (CIntType a_1) = CIntType (f a_1)
        amap f (CLongType a_1) = CLongType (f a_1)
        amap f (CFloatType a_1) = CFloatType (f a_1)
        amap f (CFloat128Type a_1) = CFloat128Type (f a_1)
        amap f (CDoubleType a_1) = CDoubleType (f a_1)
        amap f (CSignedType a_1) = CSignedType (f a_1)
        amap f (CUnsigType a_1) = CUnsigType (f a_1)
        amap f (CBoolType a_1) = CBoolType (f a_1)
        amap f (CComplexType a_1) = CComplexType (f a_1)
        amap f (CInt128Type a_1) = CInt128Type (f a_1)
        amap f (CSUType a_1 a_2) = CSUType a_1 (f a_2)
        amap f (CEnumType a_1 a_2) = CEnumType a_1 (f a_2)
        amap f (CTypeDef a_1 a_2) = CTypeDef a_1 (f a_2)
        amap f (CTypeOfExpr a_1 a_2) = CTypeOfExpr a_1 (f a_2)
        amap f (CTypeOfType a_1 a_2) = CTypeOfType a_1 (f a_2)


instance (CNode t1) => CNode (CTypeQualifier t1) where
        nodeInfo (CConstQual d) = nodeInfo d
        nodeInfo (CVolatQual d) = nodeInfo d
        nodeInfo (CRestrQual d) = nodeInfo d
        nodeInfo (CInlineQual d) = nodeInfo d
        nodeInfo (CAttrQual d) = nodeInfo d

instance (CNode t1) => Pos (CTypeQualifier t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CTypeQualifier where
        fmap _f (CConstQual a1) = CConstQual (_f a1)
        fmap _f (CVolatQual a1) = CVolatQual (_f a1)
        fmap _f (CRestrQual a1) = CRestrQual (_f a1)
        fmap _f (CInlineQual a1) = CInlineQual (_f a1)
        fmap _f (CAttrQual a1) = CAttrQual (fmap _f a1)


instance Annotated CTypeQualifier where
        annotation (CConstQual n) = n
        annotation (CVolatQual n) = n
        annotation (CRestrQual n) = n
        annotation (CInlineQual n) = n
        annotation (CAttrQual n) = annotation n
        amap f (CConstQual a_1) = CConstQual (f a_1)
        amap f (CVolatQual a_1) = CVolatQual (f a_1)
        amap f (CRestrQual a_1) = CRestrQual (f a_1)
        amap f (CInlineQual a_1) = CInlineQual (f a_1)
        amap f (CAttrQual n) = CAttrQual (amap f n)


instance (CNode t1) => CNode (CStructureUnion t1) where
        nodeInfo (CStruct _ _ _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CStructureUnion t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CStructureUnion where
        fmap _f (CStruct a1 a2 a3 a4 a5)
          = CStruct a1 a2 (fmap (fmap (fmap _f)) a3) (fmap (fmap _f) a4)
              (_f a5)


instance Annotated CStructureUnion where
        annotation (CStruct _ _ _ _ n) = n
        amap f (CStruct a_1 a_2 a_3 a_4 a_5)
          = CStruct a_1 a_2 a_3 a_4 (f a_5)


instance (CNode t1) => CNode (CEnumeration t1) where
        nodeInfo (CEnum _ _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CEnumeration t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CEnumeration where
        fmap _f (CEnum a1 a2 a3 a4)
          = CEnum a1 (fmap (fmap (fmap (fmap (fmap _f)))) a2)
              (fmap (fmap _f) a3)
              (_f a4)


instance Annotated CEnumeration where
        annotation (CEnum _ _ _ n) = n
        amap f (CEnum a_1 a_2 a_3 a_4) = CEnum a_1 a_2 a_3 (f a_4)


instance (CNode t1) => CNode (CInitializer t1) where
        nodeInfo (CInitExpr _ n) = nodeInfo n
        nodeInfo (CInitList _ n) = nodeInfo n

instance (CNode t1) => Pos (CInitializer t1) where
        posOf x = posOf (nodeInfo x)


instance Annotated CInitializer where
        annotation (CInitExpr _ n) = n
        annotation (CInitList _ n) = n
        amap f (CInitExpr a_1 a_2) = CInitExpr a_1 (f a_2)
        amap f (CInitList a_1 a_2) = CInitList a_1 (f a_2)


instance (CNode t1) => CNode (CPartDesignator t1) where
        nodeInfo (CArrDesig _ n) = nodeInfo n
        nodeInfo (CMemberDesig _ n) = nodeInfo n
        nodeInfo (CRangeDesig _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CPartDesignator t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CPartDesignator where
        fmap _f (CArrDesig a1 a2) = CArrDesig (fmap _f a1) (_f a2)
        fmap _f (CMemberDesig a1 a2) = CMemberDesig a1 (_f a2)
        fmap _f (CRangeDesig a1 a2 a3)
          = CRangeDesig (fmap _f a1) (fmap _f a2) (_f a3)


instance Annotated CPartDesignator where
        annotation (CArrDesig _ n) = n
        annotation (CMemberDesig _ n) = n
        annotation (CRangeDesig _ _ n) = n
        amap f (CArrDesig a_1 a_2) = CArrDesig a_1 (f a_2)
        amap f (CMemberDesig a_1 a_2) = CMemberDesig a_1 (f a_2)
        amap f (CRangeDesig a_1 a_2 a_3) = CRangeDesig a_1 a_2 (f a_3)


instance (CNode t1) => CNode (CAttribute t1) where
        nodeInfo (CAttr _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CAttribute t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CAttribute where
        fmap _f (CAttr a1 a2 a3) = CAttr a1 (fmap (fmap _f) a2) (_f a3)


instance Annotated CAttribute where
        annotation (CAttr _ _ n) = n
        amap f (CAttr a_1 a_2 a_3) = CAttr a_1 a_2 (f a_3)


instance (CNode t1) => CNode (CExpression t1) where
        nodeInfo (CComma _ n) = nodeInfo n
        nodeInfo (CAssign _ _ _ n) = nodeInfo n
        nodeInfo (CCond _ _ _ n) = nodeInfo n
        nodeInfo (CBinary _ _ _ n) = nodeInfo n
        nodeInfo (CCast _ _ n) = nodeInfo n
        nodeInfo (CUnary _ _ n) = nodeInfo n
        nodeInfo (CSizeofExpr _ n) = nodeInfo n
        nodeInfo (CSizeofType _ n) = nodeInfo n
        nodeInfo (CAlignofExpr _ n) = nodeInfo n
        nodeInfo (CAlignofType _ n) = nodeInfo n
        nodeInfo (CComplexReal _ n) = nodeInfo n
        nodeInfo (CComplexImag _ n) = nodeInfo n
        nodeInfo (CIndex _ _ n) = nodeInfo n
        nodeInfo (CCall _ _ n) = nodeInfo n
        nodeInfo (CMember _ _ _ n) = nodeInfo n
        nodeInfo (CVar _ n) = nodeInfo n
        nodeInfo (CConst d) = nodeInfo d
        nodeInfo (CCompoundLit _ _ n) = nodeInfo n
        nodeInfo (CStatExpr _ n) = nodeInfo n
        nodeInfo (CLabAddrExpr _ n) = nodeInfo n
        nodeInfo (CBuiltinExpr d) = nodeInfo d

instance (CNode t1) => Pos (CExpression t1) where
        posOf x = posOf (nodeInfo x)


instance Annotated CExpression where
        annotation (CComma _ n) = n
        annotation (CAssign _ _ _ n) = n
        annotation (CCond _ _ _ n) = n
        annotation (CBinary _ _ _ n) = n
        annotation (CCast _ _ n) = n
        annotation (CUnary _ _ n) = n
        annotation (CSizeofExpr _ n) = n
        annotation (CSizeofType _ n) = n
        annotation (CAlignofExpr _ n) = n
        annotation (CAlignofType _ n) = n
        annotation (CComplexReal _ n) = n
        annotation (CComplexImag _ n) = n
        annotation (CIndex _ _ n) = n
        annotation (CCall _ _ n) = n
        annotation (CMember _ _ _ n) = n
        annotation (CVar _ n) = n
        annotation (CConst n) = annotation n
        annotation (CCompoundLit _ _ n) = n
        annotation (CStatExpr _ n) = n
        annotation (CLabAddrExpr _ n) = n
        annotation (CBuiltinExpr n) = annotation n
        amap f (CComma a_1 a_2) = CComma a_1 (f a_2)
        amap f (CAssign a_1 a_2 a_3 a_4) = CAssign a_1 a_2 a_3 (f a_4)
        amap f (CCond a_1 a_2 a_3 a_4) = CCond a_1 a_2 a_3 (f a_4)
        amap f (CBinary a_1 a_2 a_3 a_4) = CBinary a_1 a_2 a_3 (f a_4)
        amap f (CCast a_1 a_2 a_3) = CCast a_1 a_2 (f a_3)
        amap f (CUnary a_1 a_2 a_3) = CUnary a_1 a_2 (f a_3)
        amap f (CSizeofExpr a_1 a_2) = CSizeofExpr a_1 (f a_2)
        amap f (CSizeofType a_1 a_2) = CSizeofType a_1 (f a_2)
        amap f (CAlignofExpr a_1 a_2) = CAlignofExpr a_1 (f a_2)
        amap f (CAlignofType a_1 a_2) = CAlignofType a_1 (f a_2)
        amap f (CComplexReal a_1 a_2) = CComplexReal a_1 (f a_2)
        amap f (CComplexImag a_1 a_2) = CComplexImag a_1 (f a_2)
        amap f (CIndex a_1 a_2 a_3) = CIndex a_1 a_2 (f a_3)
        amap f (CCall a_1 a_2 a_3) = CCall a_1 a_2 (f a_3)
        amap f (CMember a_1 a_2 a_3 a_4) = CMember a_1 a_2 a_3 (f a_4)
        amap f (CVar a_1 a_2) = CVar a_1 (f a_2)
        amap f (CConst n) = CConst (amap f n)
        amap f (CCompoundLit a_1 a_2 a_3) = CCompoundLit a_1 a_2 (f a_3)
        amap f (CStatExpr a_1 a_2) = CStatExpr a_1 (f a_2)
        amap f (CLabAddrExpr a_1 a_2) = CLabAddrExpr a_1 (f a_2)
        amap f (CBuiltinExpr n) = CBuiltinExpr (amap f n)


instance (CNode t1) => CNode (CBuiltinThing t1) where
        nodeInfo (CBuiltinVaArg _ _ n) = nodeInfo n
        nodeInfo (CBuiltinOffsetOf _ _ n) = nodeInfo n
        nodeInfo (CBuiltinTypesCompatible _ _ n) = nodeInfo n

instance (CNode t1) => Pos (CBuiltinThing t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CBuiltinThing where
        fmap _f (CBuiltinVaArg a1 a2 a3)
          = CBuiltinVaArg (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CBuiltinOffsetOf a1 a2 a3)
          = CBuiltinOffsetOf (fmap _f a1) (fmap (fmap _f) a2) (_f a3)
        fmap _f (CBuiltinTypesCompatible a1 a2 a3)
          = CBuiltinTypesCompatible (fmap _f a1) (fmap _f a2) (_f a3)


instance Annotated CBuiltinThing where
        annotation (CBuiltinVaArg _ _ n) = n
        annotation (CBuiltinOffsetOf _ _ n) = n
        annotation (CBuiltinTypesCompatible _ _ n) = n
        amap f (CBuiltinVaArg a_1 a_2 a_3) = CBuiltinVaArg a_1 a_2 (f a_3)
        amap f (CBuiltinOffsetOf a_1 a_2 a_3)
          = CBuiltinOffsetOf a_1 a_2 (f a_3)
        amap f (CBuiltinTypesCompatible a_1 a_2 a_3)
          = CBuiltinTypesCompatible a_1 a_2 (f a_3)


instance (CNode t1) => CNode (CConstant t1) where
        nodeInfo (CIntConst _ n) = nodeInfo n
        nodeInfo (CCharConst _ n) = nodeInfo n
        nodeInfo (CFloatConst _ n) = nodeInfo n
        nodeInfo (CStrConst _ n) = nodeInfo n

instance (CNode t1) => Pos (CConstant t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CConstant where
        fmap _f (CIntConst a1 a2) = CIntConst a1 (_f a2)
        fmap _f (CCharConst a1 a2) = CCharConst a1 (_f a2)
        fmap _f (CFloatConst a1 a2) = CFloatConst a1 (_f a2)
        fmap _f (CStrConst a1 a2) = CStrConst a1 (_f a2)


instance Annotated CConstant where
        annotation (CIntConst _ n) = n
        annotation (CCharConst _ n) = n
        annotation (CFloatConst _ n) = n
        annotation (CStrConst _ n) = n
        amap f (CIntConst a_1 a_2) = CIntConst a_1 (f a_2)
        amap f (CCharConst a_1 a_2) = CCharConst a_1 (f a_2)
        amap f (CFloatConst a_1 a_2) = CFloatConst a_1 (f a_2)
        amap f (CStrConst a_1 a_2) = CStrConst a_1 (f a_2)


instance (CNode t1) => CNode (CStringLiteral t1) where
        nodeInfo (CStrLit _ n) = nodeInfo n

instance (CNode t1) => Pos (CStringLiteral t1) where
        posOf x = posOf (nodeInfo x)


instance Functor CStringLiteral where
        fmap _f (CStrLit a1 a2) = CStrLit a1 (_f a2)


instance Annotated CStringLiteral where
        annotation (CStrLit _ n) = n
        amap f (CStrLit a_1 a_2) = CStrLit a_1 (f a_2)
-- GENERATED STOP
