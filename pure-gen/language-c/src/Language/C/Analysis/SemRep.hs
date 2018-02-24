{-# LANGUAGE DeriveDataTypeable  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.Syntax
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  ghc
--
-- This module contains definitions for representing C translation units.
-- In contrast to 'Language.C.Syntax.AST', the representation tries to express the semantics of
-- of a translation unit.
---------------------------------------------------------------------------------------------------
module Language.C.Analysis.SemRep(
-- * Sums of tags and identifiers
TagDef(..),typeOfTagDef,
Declaration(..),declIdent,declName,declType,declAttrs,
IdentDecl(..),objKindDescr, splitIdentDecls,
-- * Global definitions
GlobalDecls(..),emptyGlobalDecls,filterGlobalDecls,mergeGlobalDecls,
-- * Events for visitors
DeclEvent(..),
-- * Declarations and definitions
Decl(..),
ObjDef(..),isTentative,
FunDef(..),
ParamDecl(..),MemberDecl(..),
TypeDef(..),identOfTypeDef,
VarDecl(..),
-- * Declaration attributes
DeclAttrs(..),isExtDecl,
Storage(..),declStorage,ThreadLocal,Register,
Linkage(..),hasLinkage,declLinkage,
-- * Types
Type(..),
FunType(..),
ArraySize(..),
TypeDefRef(..),
TypeName(..),BuiltinType(..),
IntType(..),FloatType(..),
HasSUERef(..),HasCompTyKind(..),
CompTypeRef(..),CompType(..),typeOfCompDef,CompTyKind(..),
EnumTypeRef(..),EnumType(..),typeOfEnumDef,
Enumerator(..),
TypeQuals(..),noTypeQuals,mergeTypeQuals,
-- * Variable names
VarName(..),identOfVarName,isNoName,AsmName,
-- * Attributes (STUB, not yet analyzed)
Attr(..),Attributes,noAttributes,mergeAttributes,
-- * Statements and Expressions (STUB, aliases to Syntax)
Stmt,Expr,Initializer,AsmBlock,
)
where
import Language.C.Data
import Language.C.Syntax
import Language.C.Syntax.Constants

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Generics
import Text.PrettyPrint.HughesPJ

-- | accessor class : struct\/union\/enum names
class HasSUERef a where
    sueRef  :: a -> SUERef

-- | accessor class : composite type tags (struct or union)
class HasCompTyKind a where
    compTag :: a -> CompTyKind

-- | Composite type definitions (tags)
data TagDef =  CompDef CompType  --composite definition
             | EnumDef EnumType  --enum definition
               deriving (Typeable, Data {-! ,CNode !-})

instance HasSUERef TagDef where
    sueRef (CompDef ct) = sueRef ct
    sueRef (EnumDef et) = sueRef et

-- | return the type corresponding to a tag definition
typeOfTagDef :: TagDef -> TypeName
typeOfTagDef (CompDef comptype) =  typeOfCompDef comptype
typeOfTagDef (EnumDef enumtype) =  typeOfEnumDef enumtype

-- | All datatypes aggregating a declaration are instances of @Declaration@
class Declaration n where
    -- | get the name, type and declaration attributes of a declaration or definition
    getVarDecl :: n -> VarDecl

-- | get the declaration corresponding to a definition
declOfDef :: (Declaration n, CNode n) => n -> Decl
declOfDef def = let vd = getVarDecl def in Decl vd (nodeInfo def)

-- | get the variable identifier of a declaration (only safe if the
--    the declaration is known to have a name)
declIdent :: (Declaration n) => n -> Ident
declIdent = identOfVarName . declName
-- | get the variable name of a @Declaration@
declName ::  (Declaration n) => n -> VarName
declName = (\(VarDecl n _ _) -> n) . getVarDecl
-- | get the type of a @Declaration@
declType ::  (Declaration n) => n -> Type
declType = (\(VarDecl _ _ ty) -> ty) . getVarDecl
-- | get the declaration attributes of a @Declaration@
declAttrs :: (Declaration n) => n -> DeclAttrs
declAttrs = (\(VarDecl _ specs _) -> specs) . getVarDecl

instance (Declaration a, Declaration b) => Declaration (Either a b) where
    getVarDecl = either getVarDecl getVarDecl

-- | identifiers, typedefs and enumeration constants (namespace sum)
data IdentDecl = Declaration Decl           -- ^ object or function declaration
                     | ObjectDef ObjDef           -- ^ object definition
                     | FunctionDef FunDef         -- ^ function definition
                     | EnumeratorDef Enumerator   -- ^ definition of an enumerator
               deriving (Typeable, Data {-! ,CNode !-})

instance Declaration IdentDecl where
  getVarDecl (Declaration decl) = getVarDecl decl
  getVarDecl (ObjectDef def) = getVarDecl def
  getVarDecl (FunctionDef def) = getVarDecl def
  getVarDecl (EnumeratorDef def) = getVarDecl def

-- | textual description of the kind of an object
objKindDescr :: IdentDecl -> String
objKindDescr  (Declaration _ ) = "declaration"
objKindDescr (ObjectDef _) = "object definition"
objKindDescr (FunctionDef _) = "function definition"
objKindDescr (EnumeratorDef _) = "enumerator definition"

-- | @splitIdentDecls includeAllDecls@ splits a map of object, function and enumerator declarations and definitions into one map
-- holding declarations, and three maps for object definitions, enumerator definitions and function definitions.
-- If @includeAllDecls@ is @True@ all declarations are present in the first map, otherwise only those where no corresponding definition
-- is available.
splitIdentDecls :: Bool -> Map Ident IdentDecl -> (Map Ident Decl,
                                                ( Map Ident Enumerator,
                                                  Map Ident ObjDef,
                                                  Map Ident FunDef ) )
splitIdentDecls include_all = Map.foldWithKey (if include_all then deal else deal') (Map.empty,(Map.empty,Map.empty,Map.empty))
  where
  deal ident entry (decls,defs) = (Map.insert ident (declOfDef entry) decls, addDef ident entry defs)
  deal' ident (Declaration d) (decls,defs) = (Map.insert ident d decls,defs)
  deal' ident def (decls,defs) = (decls, addDef ident def defs)
  addDef ident entry (es,os,fs) =
    case entry of
        Declaration _   -> (es,os,fs)
        EnumeratorDef e -> (Map.insert ident e es,os,fs)
        ObjectDef o     -> (es,Map.insert ident o os,fs)
        FunctionDef f   -> (es, os,Map.insert ident f fs)


-- | global declaration\/definition table returned by the analysis
data GlobalDecls = GlobalDecls {
                     gObjs     :: Map Ident IdentDecl,
                     gTags     :: Map SUERef TagDef,
                     gTypeDefs :: Map Ident TypeDef
                   }

-- | empty global declaration table
emptyGlobalDecls :: GlobalDecls
emptyGlobalDecls = GlobalDecls Map.empty Map.empty Map.empty

-- | filter global declarations
filterGlobalDecls :: (DeclEvent -> Bool) -> GlobalDecls -> GlobalDecls
filterGlobalDecls decl_filter gmap = GlobalDecls
    {
        gObjs  = Map.filter (decl_filter . DeclEvent) (gObjs gmap),
        gTags  = Map.filter (decl_filter . TagEvent) (gTags gmap),
        gTypeDefs = Map.filter (decl_filter . TypeDefEvent) (gTypeDefs gmap)
    }
-- | merge global declarations
mergeGlobalDecls :: GlobalDecls -> GlobalDecls -> GlobalDecls
mergeGlobalDecls gmap1 gmap2 = GlobalDecls
    {
        gObjs  = Map.union (gObjs gmap1) (gObjs gmap2),
        gTags  = Map.union  (gTags gmap1) (gTags gmap2),
        gTypeDefs = Map.union (gTypeDefs gmap1) (gTypeDefs gmap2)
    }


-- * Events

-- | Declaration events
--
-- Those events are reported to callbacks, which are executed during the traversal.
data DeclEvent =
       TagEvent  TagDef
       -- ^ file-scope struct\/union\/enum event
     | DeclEvent IdentDecl
       -- ^ file-scope declaration or definition
     | ParamEvent ParamDecl
       -- ^ parameter declaration
     | LocalEvent IdentDecl
       -- ^ local variable declaration or definition
     | TypeDefEvent TypeDef
       -- ^ a type definition
     | AsmEvent AsmBlock
       -- ^ assembler block
     deriving ({-! CNode !-})

-- * Declarations and definitions

-- | Declarations, which aren't definitions
data Decl = Decl VarDecl NodeInfo
            deriving (Typeable, Data {-! ,CNode !-})

instance Declaration Decl where
    getVarDecl   (Decl vd _) =  vd

-- | Object Definitions
--
-- An object definition is a declaration together with an initializer.
--
-- If the initializer is missing, it is a tentative definition, i.e. a
-- definition which might be overriden later on.
data ObjDef = ObjDef VarDecl (Maybe Initializer) NodeInfo
             deriving (Typeable, Data {-! ,CNode !-})
instance Declaration ObjDef where
    getVarDecl  (ObjDef vd _ _) =  vd

-- | Returns @True@ if the given object definition is tentative.
isTentative :: ObjDef -> Bool
isTentative (ObjDef decl init_opt _) | isExtDecl decl = maybe True (const False) init_opt
                                     | otherwise = False

-- | Function definitions
--
-- A function definition is a declaration together with a statement (the function body).
data FunDef = FunDef VarDecl Stmt NodeInfo
             deriving (Typeable, Data {-! ,CNode !-})
instance Declaration FunDef where
    getVarDecl (FunDef vd _ _) = vd


-- | Parameter declaration
data ParamDecl = ParamDecl VarDecl NodeInfo
               | AbstractParamDecl VarDecl NodeInfo
    deriving (Typeable, Data {-! ,CNode !-} )

instance Declaration ParamDecl where
  getVarDecl (ParamDecl vd _) = vd
  getVarDecl (AbstractParamDecl vd _) = vd

-- | Struct\/Union member declaration
data MemberDecl = MemberDecl VarDecl (Maybe Expr) NodeInfo
                  -- ^ @MemberDecl vardecl bitfieldsize node@
                | AnonBitField Type Expr NodeInfo
                  -- ^ @AnonBitField typ size@
    deriving (Typeable, Data {-! ,CNode !-} )

instance Declaration MemberDecl where
  getVarDecl (MemberDecl vd _ _) = vd
  getVarDecl (AnonBitField ty _ _) = VarDecl NoName (DeclAttrs False NoStorage []) ty

-- | @typedef@ definitions.
--
-- The identifier is a new name for the given type.
data TypeDef = TypeDef Ident Type Attributes NodeInfo
               deriving (Typeable, Data {-! ,CNode !-} )

-- | return the idenitifier of a @typedef@
identOfTypeDef :: TypeDef -> Ident
identOfTypeDef (TypeDef ide _ _ _) = ide

-- | Generic variable declarations
data VarDecl = VarDecl VarName DeclAttrs Type
              deriving (Typeable, Data)

instance Declaration VarDecl where
  getVarDecl = id

-- @isExtDecl d@ returns true if the declaration has /linkage/
isExtDecl :: (Declaration n) => n -> Bool
isExtDecl = hasLinkage . declStorage

-- | Declaration attributes of the form @DeclAttrs isInlineFunction storage linkage attrs@
--
-- They specify the storage and linkage of a declared object.
data DeclAttrs = DeclAttrs Bool Storage Attributes
                 -- ^ @DeclAttrs inline storage attrs@
               deriving (Typeable, Data)

-- | get the 'Storage' of a declaration
declStorage :: (Declaration d) => d -> Storage
declStorage d = case declAttrs d of (DeclAttrs _ st _) -> st

-- In C we have
--  Identifiers can either have internal, external or no linkage
--  (same object everywhere, same object within the translation unit, unique).
--  * top-level identifiers
--      static : internal linkage (objects and function defs)
--      extern : linkage of prior declaration (if specified), external linkage otherwise
--      no-spec: external linkage
--  * storage duration
--      * static storage duration: objects with external or internal linkage, or local ones with the static keyword
--      * automatic storage duration: otherwise (register)
-- See http://publications.gbdirect.co.uk/c_book/chapter8/declarations_and_definitions.html, Table 8.1, 8.2

-- | Storage duration and linkage of a variable
data Storage  =  NoStorage                  -- ^ no storage
               | Auto Register              -- ^ automatic storage (optional: register)
               | Static Linkage ThreadLocal -- ^ static storage, linkage spec and thread local specifier (gnu c)
               | FunLinkage Linkage         -- ^ function, either internal or external linkage
               deriving (Typeable, Data, Show, Eq, Ord)

type ThreadLocal = Bool
type Register    = Bool

-- | Linkage: Either no linkage, internal to the translation unit or external
data Linkage = NoLinkage | InternalLinkage | ExternalLinkage
               deriving (Typeable, Data, Show, Eq, Ord)

-- | return @True@ if the object has linkage
hasLinkage :: Storage -> Bool
hasLinkage (Auto _) = False
hasLinkage (Static NoLinkage _) = False
hasLinkage _ = True

-- | Get the linkage of a definition
declLinkage :: (Declaration d) => d -> Linkage
declLinkage decl =
    case declStorage decl of
        NoStorage -> undefined
        Auto _ -> NoLinkage
        Static linkage _ -> linkage
        FunLinkage linkage -> linkage


-- * types

-- | types of C objects
data Type =
       DirectType TypeName TypeQuals Attributes
     -- ^ a non-derived type
     | PtrType Type TypeQuals Attributes
     -- ^ pointer type
     | ArrayType Type ArraySize TypeQuals Attributes
     -- ^ array type
     | FunctionType FunType Attributes
     -- ^ function type
     | TypeDefType TypeDefRef TypeQuals Attributes
     -- ^ a defined type
     deriving (Typeable, Data)

-- | Function types are of the form @FunType return-type params isVariadic@.
--
-- If the parameter types aren't yet known, the function has type @FunTypeIncomplete type attrs@.
data FunType = FunType Type [ParamDecl] Bool
            |  FunTypeIncomplete Type
               deriving (Typeable, Data)

-- | An array type may either have unknown size or a specified array size, the latter either variable or constant.
-- Furthermore, when used as a function parameters, the size may be qualified as /static/.
-- In a function prototype, the size may be `Unspecified variable size' (@[*]@).
data ArraySize =  UnknownArraySize Bool
                -- ^ @UnknownArraySize is-starred@
                | ArraySize Bool Expr
                -- ^ @FixedSizeArray is-static size-expr@
               deriving (Typeable, Data)

-- | normalized type representation
data TypeName =
      TyVoid
    | TyIntegral IntType
    | TyFloating FloatType
    | TyComplex  FloatType
    | TyComp CompTypeRef
    | TyEnum EnumTypeRef
    | TyBuiltin BuiltinType
    deriving (Typeable, Data)

-- | Builtin type (va_list, anything)
data BuiltinType = TyVaList
                 | TyAny
                   deriving (Typeable, Data)

-- | typdef references
-- If the actual type is known, it is attached for convenience
data TypeDefRef = TypeDefRef Ident (Maybe Type) NodeInfo
               deriving (Typeable, Data {-! ,CNode !-})

-- | integral types (C99 6.7.2.2)
data IntType =
      TyBool
    | TyChar
    | TySChar
    | TyUChar
    | TyShort
    | TyUShort
    | TyInt
    | TyUInt
    | TyInt128
    | TyUInt128
    | TyLong
    | TyULong
    | TyLLong
    | TyULLong
    deriving (Typeable, Data, Eq, Ord)

instance Show IntType where
    show TyBool = "_Bool"
    show TyChar = "char"
    show TySChar = "signed char"
    show TyUChar = "unsigned char"
    show TyShort = "short"
    show TyUShort = "unsigned short"
    show TyInt = "int"
    show TyUInt = "unsigned int"
    show TyInt128 = "__int128"
    show TyUInt128 = "unsigned __int128"
    show TyLong = "long"
    show TyULong = "unsigned long"
    show TyLLong = "long long"
    show TyULLong = "unsigned long long"

-- | floating point type (C99 6.7.2.2)
data FloatType =
      TyFloat
    | TyDouble
    | TyLDouble
    deriving (Typeable, Data, Eq, Ord)

instance Show FloatType where
    show TyFloat = "float"
    show TyDouble = "double"
    show TyLDouble = "long double"

-- | composite type declarations
data CompTypeRef = CompTypeRef SUERef CompTyKind NodeInfo
                    deriving (Typeable, Data {-! ,CNode !-})

instance HasSUERef  CompTypeRef where sueRef  (CompTypeRef ref _ _) = ref
instance HasCompTyKind CompTypeRef where compTag (CompTypeRef _ tag _)  = tag

data EnumTypeRef = EnumTypeRef SUERef NodeInfo
    deriving (Typeable, Data {-! ,CNode !-})
instance HasSUERef  EnumTypeRef where sueRef  (EnumTypeRef ref _) = ref

-- | Composite type (struct or union).
data CompType =  CompType SUERef CompTyKind [MemberDecl] Attributes NodeInfo
                 deriving (Typeable, Data {-! ,CNode !-} )
instance HasSUERef  CompType where sueRef  (CompType ref _ _ _ _) = ref
instance HasCompTyKind CompType where compTag (CompType _ tag _ _ _) = tag

-- | return the type of a composite type definition
typeOfCompDef :: CompType -> TypeName
typeOfCompDef (CompType ref tag _ _ _) = TyComp (CompTypeRef ref tag undefNode)

-- | a tag to determine wheter we refer to a @struct@ or @union@, see 'CompType'.
data CompTyKind =  StructTag
                 | UnionTag
    deriving (Eq,Ord,Typeable,Data)

instance Show CompTyKind where
    show StructTag = "struct"
    show UnionTag  = "union"

-- | Representation of C enumeration types
data EnumType = EnumType SUERef [Enumerator] Attributes NodeInfo
                 -- ^ @EnumType name enumeration-constants attrs node@
                 deriving (Typeable, Data {-! ,CNode !-} )

instance HasSUERef EnumType where sueRef  (EnumType ref _ _ _) = ref

-- | return the type of an enum definition
typeOfEnumDef :: EnumType -> TypeName
typeOfEnumDef (EnumType ref _ _ _) = TyEnum (EnumTypeRef ref undefNode)

-- | An Enumerator consists of an identifier, a constant expressions and the link to its type
data Enumerator = Enumerator Ident Expr EnumType NodeInfo
                  deriving (Typeable, Data {-! ,CNode !-})
instance Declaration Enumerator where
  getVarDecl (Enumerator ide _ enumty _) =
    VarDecl
      (VarName ide Nothing)
      (DeclAttrs False NoStorage [])
      (DirectType (typeOfEnumDef enumty) noTypeQuals noAttributes)

-- | Type qualifiers: constant, volatile and restrict
data TypeQuals = TypeQuals { constant :: Bool, volatile :: Bool, restrict :: Bool }
    deriving (Typeable, Data)

-- | no type qualifiers
noTypeQuals :: TypeQuals
noTypeQuals = TypeQuals False False False

-- | merge (/&&/) two type qualifier sets
mergeTypeQuals :: TypeQuals -> TypeQuals -> TypeQuals
mergeTypeQuals (TypeQuals c1 v1 r1) (TypeQuals c2 v2 r2) = TypeQuals (c1 && c2) (v1 && v2) (r1 && r2)

-- * initializers

-- | 'Initializer' is currently an alias for 'CInit'.
--
-- We're planning a normalized representation, but this depends on the implementation of
-- constant expression evaluation
type Initializer = CInit

-- | Normalized C Initializers
-- * If the expression has scalar type, the initializer is an expression
-- * If the expression has struct type, the initializer is a map from designators to initializers
-- * If the expression has array type, the initializer is a list of values
-- Not implemented yet, as it depends on constant expression evaluation


-- * names and attributes

-- | @VarName name assembler-name@ is a name of an declared object
data VarName =  VarName Ident (Maybe AsmName)
              | NoName
               deriving (Typeable, Data)
identOfVarName :: VarName -> Ident
identOfVarName NoName            = error "identOfVarName: NoName"
identOfVarName (VarName ident _) = ident

isNoName :: VarName -> Bool
isNoName NoName = True
isNoName _ = False

-- | Top level assembler block (alias for @CStrLit@)
type AsmBlock = CStrLit

-- | Assembler name (alias for @CStrLit@)
type AsmName = CStrLit

-- | @__attribute__@ annotations
--
-- Those are of the form @Attr attribute-name attribute-parameters@,
-- and serve as generic properties of some syntax tree elements.
--
-- Some examples:
--
-- * labels can be attributed with /unused/ to indicate that their not used
--
-- * struct definitions can be attributed with /packed/ to tell the compiler to use the most compact representation
--
-- * declarations can be attributed with /deprecated/
--
-- * function declarations can be attributes with /noreturn/ to tell the compiler that the function will never return,
--
-- * or with /const/ to indicate that it is a pure function
--
-- /TODO/: ultimatively, we want to parse attributes and represent them in a typed way
data Attr = Attr Ident [Expr] NodeInfo
            deriving (Typeable, Data {-! ,CNode !-})

type Attributes = [Attr]

-- |Empty attribute list
noAttributes :: Attributes
noAttributes = []

-- |Merge attribute lists
-- /TODO/: currently does not remove duplicates
mergeAttributes :: Attributes -> Attributes -> Attributes
mergeAttributes = (++)

-- * statements and expressions (Type aliases)

-- | 'Stmt' is an alias for 'CStat' (Syntax)
type Stmt = CStat
-- | 'Expr' is currently an alias for 'CExpr' (Syntax)
type Expr = CExpr
-- GENERATED START


instance CNode TagDef where
        nodeInfo (CompDef d) = nodeInfo d
        nodeInfo (EnumDef d) = nodeInfo d

instance Pos TagDef where
        posOf x = posOf (nodeInfo x)


instance CNode IdentDecl where
        nodeInfo (Declaration d) = nodeInfo d
        nodeInfo (ObjectDef d) = nodeInfo d
        nodeInfo (FunctionDef d) = nodeInfo d
        nodeInfo (EnumeratorDef d) = nodeInfo d

instance Pos IdentDecl where
        posOf x = posOf (nodeInfo x)


instance CNode DeclEvent where
        nodeInfo (TagEvent d) = nodeInfo d
        nodeInfo (DeclEvent d) = nodeInfo d
        nodeInfo (ParamEvent d) = nodeInfo d
        nodeInfo (LocalEvent d) = nodeInfo d
        nodeInfo (TypeDefEvent d) = nodeInfo d
        nodeInfo (AsmEvent d) = nodeInfo d

instance Pos DeclEvent where
        posOf x = posOf (nodeInfo x)


instance CNode Decl where
        nodeInfo (Decl _ n) = n

instance Pos Decl where
        posOf x = posOf (nodeInfo x)


instance CNode ObjDef where
        nodeInfo (ObjDef _ _ n) = n

instance Pos ObjDef where
        posOf x = posOf (nodeInfo x)


instance CNode FunDef where
        nodeInfo (FunDef _ _ n) = n

instance Pos FunDef where
        posOf x = posOf (nodeInfo x)


instance CNode ParamDecl where
        nodeInfo (ParamDecl _ n) = n
        nodeInfo (AbstractParamDecl _ n) = n

instance Pos ParamDecl where
        posOf x = posOf (nodeInfo x)


instance CNode MemberDecl where
        nodeInfo (MemberDecl _ _ n) = n
        nodeInfo (AnonBitField _ _ n) = n

instance Pos MemberDecl where
        posOf x = posOf (nodeInfo x)


instance CNode TypeDef where
        nodeInfo (TypeDef _ _ _ n) = n

instance Pos TypeDef where
        posOf x = posOf (nodeInfo x)


instance CNode TypeDefRef where
        nodeInfo (TypeDefRef _ _ n) = n

instance Pos TypeDefRef where
        posOf x = posOf (nodeInfo x)


instance CNode CompTypeRef where
        nodeInfo (CompTypeRef _ _ n) = n

instance Pos CompTypeRef where
        posOf x = posOf (nodeInfo x)


instance CNode EnumTypeRef where
        nodeInfo (EnumTypeRef _ n) = n

instance Pos EnumTypeRef where
        posOf x = posOf (nodeInfo x)


instance CNode CompType where
        nodeInfo (CompType _ _ _ _ n) = n

instance Pos CompType where
        posOf x = posOf (nodeInfo x)


instance CNode EnumType where
        nodeInfo (EnumType _ _ _ n) = n

instance Pos EnumType where
        posOf x = posOf (nodeInfo x)


instance CNode Enumerator where
        nodeInfo (Enumerator _ _ _ n) = n

instance Pos Enumerator where
        posOf x = posOf (nodeInfo x)


instance CNode Attr where
        nodeInfo (Attr _ _ n) = n

instance Pos Attr where
        posOf x = posOf (nodeInfo x)
-- GENERATED STOP
