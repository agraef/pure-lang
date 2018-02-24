{-# LANGUAGE PatternGuards, DeriveDataTypeable  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.DefTable
-- Copyright   :  (c) 2008 Benedikt Huber
--                  based on code from c2hs
--                (c) [1999..2001] Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  ghc
--
-- This module manages symbols in local and global scopes.
--
-- There are four different kind of identifiers: ordinary identifiers (henceforth
-- simply called `identifier'), tag names (names of struct\/union\/enum types),
-- labels and structure members.
-----------------------------------------------------------------------------
module Language.C.Analysis.DefTable (
    IdentEntry, identOfTyDecl,
    TagEntry, TagFwdDecl(..),
    DefTable(..),
    emptyDefTable,
    globalDefs,
    inFileScope,
    enterFunctionScope,leaveFunctionScope,enterBlockScope,leaveBlockScope,
    enterMemberDecl,leaveMemberDecl,
    DeclarationStatus(..),declStatusDescr,
    defineTypeDef, defineGlobalIdent, defineScopedIdent, defineScopedIdentWhen,
    declareTag,defineTag,defineLabel,lookupIdent,
    lookupTag,lookupLabel,lookupIdentInner,lookupTagInner,
    insertType, lookupType,
    mergeDefTable
)
where
import Language.C.Data
import Language.C.Analysis.NameSpaceMap
import Language.C.Analysis.SemRep

import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap, union)
import qualified Data.IntMap as IntMap
import Data.Generics

{- Name spaces, scopes and contexts [Scopes]

 In C, there are 4 categories of identifiers:

  * labels
  * tag names (@(struct|union|enum) tag-name@), where /all/ tag names live in one namespace
  * members of structures and unions
  * ordinary identifiers, denoting objects, functions, typeDefs and enumeration constants

 There are 4 kind of scopes:

  * file scope: outside of parameter lists and blocks
  * function prototype scope
  * function scope: labels are visible within the entire function, and declared implicitely
  * block scope

 While function scope is irrelevant for variable declarations, they might also appear in member declarations.
 Therefore, there are also 4 kinds of contexts where a variable might be declared:

  * File Scope Context: external declaration \/ definition
  * Block Scope Context: either external or local definition
  * Function prototype scope context
  * Member Declaration context

 See
   <http://www.embedded.com/design/206901036>
   C99 6
-}

-- | All ordinary identifiers map to 'IdenTyDecl': either a typedef or a object\/function\/enumerator
type IdentEntry = Either TypeDef IdentDecl

identOfTyDecl :: IdentEntry -> Ident
identOfTyDecl = either identOfTypeDef declIdent

data TagFwdDecl = CompDecl CompTypeRef
                | EnumDecl EnumTypeRef
instance HasSUERef TagFwdDecl where
  sueRef (CompDecl ctr) = sueRef ctr
  sueRef (EnumDecl etr) = sueRef etr
instance CNode TagFwdDecl where
  nodeInfo (CompDecl ctr) = nodeInfo ctr
  nodeInfo (EnumDecl etr) = nodeInfo etr

-- | Tag names map to forward declarations or definitions of struct\/union\/enum types
type TagEntry = Either TagFwdDecl TagDef

-- | Table holding current definitions
data DefTable = DefTable
    {
        identDecls   :: NameSpaceMap Ident IdentEntry,     -- ^ declared `ordinary identifiers'
        tagDecls   :: NameSpaceMap SUERef TagEntry,        -- ^ declared struct/union/enum  tags
        labelDefs  :: NameSpaceMap Ident Ident,            -- ^ defined labels
        memberDecls :: NameSpaceMap Ident MemberDecl,      -- ^ member declarations (only local)
        refTable   :: IntMap Name,                         -- ^ link names with definitions
        typeTable  :: IntMap Type
    }

-- | empty definition table, with all name space maps in global scope
emptyDefTable :: DefTable
emptyDefTable = DefTable nameSpaceMap nameSpaceMap nameSpaceMap nameSpaceMap IntMap.empty IntMap.empty

-- | get the globally defined entries of a definition table
globalDefs :: DefTable -> GlobalDecls
globalDefs deftbl = Map.foldWithKey insertDecl (GlobalDecls e gtags e) (globalNames $ identDecls deftbl)
    where
    e = Map.empty
    (_fwd_decls,gtags) = Map.mapEither id $ globalNames (tagDecls deftbl)
    insertDecl ident (Left tydef) ds = ds { gTypeDefs = Map.insert ident tydef (gTypeDefs ds)}
    insertDecl ident (Right obj) ds = ds { gObjs = Map.insert ident obj (gObjs ds) }

inFileScope :: DefTable -> Bool
inFileScope dt = not (hasLocalNames (identDecls dt) || hasLocalNames (labelDefs dt))

leaveScope_ :: (Ord k) => NameSpaceMap k a -> NameSpaceMap k a
leaveScope_ = fst . leaveScope

enterLocalScope :: DefTable -> DefTable
enterLocalScope deftbl = deftbl {
        identDecls = enterNewScope (identDecls deftbl),
        tagDecls = enterNewScope (tagDecls deftbl)
    }
leaveLocalScope :: DefTable -> DefTable
leaveLocalScope deftbl = deftbl {
                        identDecls = leaveScope_ (identDecls deftbl),
                        tagDecls = leaveScope_ (tagDecls deftbl)
                       }

-- | Enter function scope (AND the corresponding block scope)
enterFunctionScope :: DefTable -> DefTable
enterFunctionScope deftbl = enterLocalScope  $ deftbl { labelDefs = enterNewScope (labelDefs deftbl) }

-- | Leave function scope, and return the associated DefTable.
--   Error if not in function scope.
leaveFunctionScope :: DefTable -> DefTable
leaveFunctionScope deftbl = leaveLocalScope $ deftbl { labelDefs = leaveScope_ (labelDefs deftbl) }

-- | Enter new block scope
enterBlockScope :: DefTable -> DefTable
enterBlockScope deftbl = enterLocalScope $ deftbl { labelDefs = enterNewScope (labelDefs deftbl) }

-- | Leave innermost block scope
leaveBlockScope :: DefTable -> DefTable
leaveBlockScope deftbl = leaveLocalScope $ deftbl { labelDefs = leaveScope_ (labelDefs deftbl) }

-- | Enter new member declaration scope
enterMemberDecl :: DefTable -> DefTable
enterMemberDecl deftbl = deftbl { memberDecls = enterNewScope (memberDecls deftbl) }

-- | Leave innermost member declaration scope
leaveMemberDecl :: DefTable -> ([MemberDecl], DefTable)
leaveMemberDecl deftbl =
    let (decls',members) = leaveScope (memberDecls deftbl)
    in (,) (map snd members)
           (deftbl { memberDecls = decls' })

-- * declarations

-- | Status of a declaration
data DeclarationStatus t =
      NewDecl         -- ^ new entry
    | Redeclared t    -- ^ old def was overwritten
    | KeepDef t       -- ^ new def was discarded
    | Shadowed t      -- ^ new def shadows one in outer scope
    | KindMismatch t  -- ^ kind mismatch
    deriving (Data,Typeable)
declStatusDescr :: DeclarationStatus t -> String
declStatusDescr NewDecl = "new"
declStatusDescr (Redeclared _) = "redeclared"
declStatusDescr (KeepDef _) = "keep old"
declStatusDescr (Shadowed _) = "shadowed"
declStatusDescr (KindMismatch _) = "kind mismatch"

compatIdentEntry :: IdentEntry -> IdentEntry -> Bool
compatIdentEntry (Left _tydef) = either (const True) (const False)
compatIdentEntry (Right def) = either (const False) $
  \other_def -> case (def,other_def) of
                  (EnumeratorDef _, EnumeratorDef _) -> True
                  (EnumeratorDef _, _) -> True
                  (_, EnumeratorDef _) -> True
                  (_,_) -> True

data TagEntryKind = CompKind CompTyKind | EnumKind
                    deriving (Eq,Ord)
instance Show TagEntryKind where
  show (CompKind ctk) = show ctk
  show EnumKind = "enum"

-- | @sameTagKind ty1 ty2@ returns @True@ if @ty1,ty2@ are the same kind of tag (struct,union or enum)
tagKind :: TagEntry -> TagEntryKind
tagKind (Left (CompDecl cd)) = CompKind (compTag cd)
tagKind (Left (EnumDecl _)) = EnumKind
tagKind (Right (CompDef cd)) = CompKind (compTag cd)
tagKind (Right (EnumDef _)) =  EnumKind

compatTagEntry :: TagEntry -> TagEntry -> Bool
compatTagEntry  te1 te2 = tagKind te1 == tagKind te2

defRedeclStatus :: (t -> t -> Bool) -> t -> Maybe t -> DeclarationStatus t
defRedeclStatus sameKind def oldDecl =
    case oldDecl of
        Just def' | def `sameKind` def' -> Redeclared def'
                  | otherwise           -> KindMismatch def'
        Nothing                         -> NewDecl

defRedeclStatusLocal :: (Ord k) =>
                        (t -> t -> Bool) -> k -> t -> Maybe t -> NameSpaceMap k t -> DeclarationStatus t
defRedeclStatusLocal sameKind ident def oldDecl nsm =
    case defRedeclStatus sameKind def oldDecl of
        NewDecl -> case lookupName nsm ident of
                     Just shadowed -> Shadowed shadowed
                     Nothing       -> NewDecl
        redecl  -> redecl

defineTypeDef :: Ident -> TypeDef -> DefTable -> (DeclarationStatus IdentEntry, DefTable)
defineTypeDef ident tydef deftbl =
  (defRedeclStatus compatIdentEntry (Left tydef) oldDecl, deftbl { identDecls = decls' })
  where
  (decls', oldDecl) = defLocal (identDecls deftbl) ident (Left tydef)

-- | declare\/define a global object\/function\/typeDef
--
--  returns @Redeclared def@ if there is already an object\/function\/typeDef
--  in global scope, or @DifferentKindRedec def@ if the old declaration is of a different kind.
defineGlobalIdent :: Ident -> IdentDecl -> DefTable -> (DeclarationStatus IdentEntry, DefTable)
defineGlobalIdent ident def deftbl =
    (defRedeclStatus compatIdentEntry (Right def) oldDecl, deftbl { identDecls = decls' })
    where
    (decls',oldDecl) = defGlobal (identDecls deftbl) ident (Right def)
-- | declare\/define a object\/function\/typeDef with lexical scope
--
--  returns @Redeclared def@ or @DifferentKindRedec def@  if there is already an object\/function\/typeDef
--  in the same scope.
defineScopedIdent :: Ident -> IdentDecl -> DefTable -> (DeclarationStatus IdentEntry, DefTable)
defineScopedIdent = defineScopedIdentWhen (const True)

-- | declare\/define a object\/function\/typeDef with lexical scope, if the given predicate holds on the old
--   entry.
--
--  returns @Keep old_def@ if the old definition shouldn't be overwritten, and otherwise @Redeclared def@ or
--  @DifferentKindRedecl def@  if there is already an object\/function\/typeDef in the same scope.
defineScopedIdentWhen :: (IdentDecl -> Bool) -> Ident -> IdentDecl -> DefTable ->
                           (DeclarationStatus IdentEntry, DefTable)
defineScopedIdentWhen override_def ident def deftbl
    = (redecl_status, deftbl { identDecls = decls' })
    where
    new_def = Right def
    old_decls = identDecls deftbl
    old_decl_opt = lookupInnermostScope old_decls ident
    (decls',redecl_status)  | (Just old_decl) <- old_decl_opt, not (old_decl `compatIdentEntry` new_def)
                              = (new_decls, KindMismatch old_decl)
                            | maybe True doOverride old_decl_opt
                              = (new_decls, redeclStatus' old_decl_opt)
                            | otherwise
                              = (old_decls, maybe NewDecl KeepDef old_decl_opt)
    new_decls = fst (defLocal old_decls ident new_def)
    doOverride (Left _) = False
    doOverride (Right old_def) = (override_def old_def)
    redeclStatus' overriden_decl = defRedeclStatusLocal compatIdentEntry ident new_def overriden_decl old_decls

-- | declare a tag (fwd decl in case the struct name isn't defined yet)
declareTag :: SUERef -> TagFwdDecl -> DefTable -> (DeclarationStatus TagEntry, DefTable)
declareTag sueref decl deftbl =
  case lookupTag sueref deftbl of
    Nothing -> (NewDecl, deftbl { tagDecls = fst $ defLocal (tagDecls deftbl) sueref (Left decl) })
    Just old_def | tagKind old_def == tagKind (Left decl) ->  (KeepDef old_def, deftbl)
                 | otherwise -> (KindMismatch old_def, deftbl)

-- | define a tag
defineTag :: SUERef -> TagDef -> DefTable -> (DeclarationStatus TagEntry, DefTable)
defineTag sueref def deftbl =
    (redeclStatus, deftbl { tagDecls = decls'})
    where
    (decls',olddecl) = defLocal (tagDecls deftbl) sueref (Right def)
    redeclStatus =
      case olddecl of
        Just fwd_decl@(Left decl) | tagKind fwd_decl == tagKind (Right def) -> NewDecl -- should be NewDef
                                  | otherwise -> KindMismatch fwd_decl
        _ -> defRedeclStatusLocal compatTagEntry sueref (Right def) olddecl (tagDecls deftbl)

-- | define a label
-- Return the old label if it is already defined in this function's scope
defineLabel :: Ident -> DefTable -> (DeclarationStatus Ident, DefTable)
defineLabel ident deftbl =
    let (labels',old_label) = defLocal (labelDefs deftbl) ident ident
    in  (maybe NewDecl Redeclared old_label, deftbl { labelDefs = labels' })

-- | lookup identifier (object, function, typeDef, enumerator)
lookupIdent :: Ident -> DefTable -> Maybe IdentEntry
lookupIdent ident deftbl = lookupName (identDecls deftbl) ident

-- | lookup tag
lookupTag :: SUERef -> DefTable -> Maybe TagEntry
lookupTag sue_ref deftbl = lookupName (tagDecls deftbl) sue_ref

-- | lookup label
lookupLabel :: Ident -> DefTable -> Maybe Ident
lookupLabel ident deftbl = lookupName (labelDefs deftbl) ident

-- | lookup an object in the innermost scope
lookupIdentInner :: Ident -> DefTable -> Maybe IdentEntry
lookupIdentInner ident deftbl = lookupInnermostScope (identDecls deftbl) ident

-- | lookup an identifier in the innermost scope
lookupTagInner :: SUERef -> DefTable -> Maybe TagEntry
lookupTagInner sue_ref deftbl = lookupInnermostScope (tagDecls deftbl) sue_ref

-- | Record the type of a node.
insertType :: DefTable -> Name -> Type -> DefTable
insertType dt n t = dt { typeTable = IntMap.insert (nameId n) t (typeTable dt) }

-- | Lookup the type of a node.
lookupType :: DefTable -> Name -> Maybe Type
lookupType dt n = IntMap.lookup (nameId n) (typeTable dt)

-- | Merge two DefTables. If both tables contain an entry for a given
--   key, they must agree on its value.
mergeDefTable :: DefTable -> DefTable -> DefTable
mergeDefTable (DefTable i1 t1 l1 m1 r1 tt1) (DefTable i2 t2 l2 m2 r2 tt2) =
  DefTable
  (mergeNameSpace i1 i2)
  (mergeNameSpace t1 t2)
  (mergeNameSpace l1 l2)
  (mergeNameSpace m1 m2)
  (union r1 r2)
  (union tt1 tt2)

