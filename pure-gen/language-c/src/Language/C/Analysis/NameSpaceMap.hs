-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.NameSpaceMap
-- Copyright   :  (c) [1995..1999] Manuel M. T. Chakravarty
--                (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  portable
--
-- This module manages name spaces.
--
--  * A name space map associates identifiers with their definition.
--
--  * Each name space map is organized in a hierarchical way using the notion of
--    scopes. A name space map, at any moment, always has a global scope and may
--    have several local scopes. Definitions in inner scopes hide definitions
--    of the same identifier in outer scopes.
--
module Language.C.Analysis.NameSpaceMap (
    -- * name space maps
    NameSpaceMap, nameSpaceMap, nsMapToList,
    globalNames,localNames,hasLocalNames,
    -- * scope modification
    defGlobal,
    enterNewScope, leaveScope,
    defLocal,
    lookupName,lookupGlobal,lookupInnermostScope,
    mergeNameSpace
    )
where
import Prelude hiding (lookup)
import qualified Prelude
import qualified Data.Map as Map (empty, insert, lookup, toList, union)
import qualified Data.List as List (unionBy)
import Data.Map   (Map)
import Language.C.Data.Ident     (Ident)

{-
C Namespaces and scopes:


-}

-- DevDocs:
--
-- * the definitions in the global scope are stored in a finite map, because
--   they tend to be a lot.
--
-- * the definitions of the local scopes are stored in a single list, usually
--   they are not very many and the definitions entered last are the most
--   frequently accessed ones; the list structure naturally hides older
--   definitions, i.e., definitions from outer scopes; adding new definitions
--   is done in time proportinal to the current size of the scope; removing a
--   scope is done in constant time (and the definitions of a scope can be
--   returned as a result of leaving the scope); lookup is proportional to the
--   number of definitions in the local scopes and the logarithm of the number
--   of definitions in the global scope -- i.e., efficiency relies on a
--   relatively low number of local definitions together with frequent lookup
--   of the most recently defined local identifiers
--

-- | @NameSpaceMap a@ is a Map from identifiers to @a@, which manages
-- global and local name spaces.
data NameSpaceMap k v = NsMap (Map k v)  -- defs in global scope
                              [[(k, v)]] -- stack of local scopes
globalNames :: (Ord k) => NameSpaceMap k v -> Map k v
globalNames (NsMap g _) = g
hasLocalNames :: NameSpaceMap k v -> Bool
hasLocalNames (NsMap _ l) = not (null l)
localNames :: (Ord k) => NameSpaceMap k v -> [[(k,v)]]
localNames (NsMap _ l) = l

-- | create a name space
nameSpaceMap :: (Ord k) => NameSpaceMap k v
nameSpaceMap  = NsMap Map.empty []



-- | Add global definition
--
-- @(ns',oldDef) = defGlobal ns ident def@
--   adds a global definition @ident := def@ to the namespace.
--   It returns the modified namespace @ns'@. If the identifier is
--   already declared in the global namespace, the definition is overwritten
--   and the old definition @oldDef@ is returned.
defGlobal :: (Ord k) => NameSpaceMap k a -> k -> a -> (NameSpaceMap k a, Maybe a)
defGlobal (NsMap gs lss) ident def
    = (NsMap (Map.insert ident def gs) lss, Map.lookup ident gs)

-- | Enter new local scope
--
-- @ns' = enterNewScope ns@ creates and enters a new local scope.
enterNewScope :: (Ord k) => NameSpaceMap k a -> NameSpaceMap k a
enterNewScope (NsMap gs lss)  = NsMap gs ([]:lss)

-- | Leave innermost local scope
--
-- @(ns',defs) = leaveScope ns@ pops leaves the innermost local scope.
--  and returns its definitions
leaveScope :: (Ord k) => NameSpaceMap k a -> (NameSpaceMap k a, [(k, a)])
leaveScope (NsMap _ [])         = error "NsMaps.leaveScope: No local scope!"
leaveScope (NsMap gs (ls:lss))  = (NsMap gs lss, ls)

-- | Add local definition
--
-- @(ns',oldDef) = defLocal ns ident def@ adds the local definition
--   @ident := def@ to the innermost local scope, if there is a local scope,
--     and to the global scope otherwise.
--   It returns the modified name space @ns'@ and the old  binding of
--   the identifier @oldDef@, which is overwritten.
defLocal :: (Ord k) => NameSpaceMap k a -> k -> a -> (NameSpaceMap k a, Maybe a)
defLocal ns@(NsMap _ []) ident def = defGlobal ns ident def
defLocal (NsMap    gs (ls:lss)) ident def =
  (NsMap gs (((ident, def):ls):lss),
   Prelude.lookup ident ls)

-- | Search for a definition
--
-- @def = find ns ident@ returns the definition in some scope (inner to outer),
-- if there is one.
lookupName :: (Ord k) => NameSpaceMap k a -> k -> Maybe a
lookupName ns@(NsMap _ localDefs) ident
    = case (lookupLocal localDefs) of
        Nothing  -> lookupGlobal ns ident
        Just def -> Just def
  where
    lookupLocal []       = Nothing
    lookupLocal (ls:lss) = case (Prelude.lookup ident ls) of
                        Nothing  -> lookupLocal lss
                        Just def -> Just def

lookupGlobal :: (Ord k) => NameSpaceMap k a -> k -> Maybe a
lookupGlobal (NsMap gs _) ident = Map.lookup ident gs

lookupInnermostScope :: (Ord k) => NameSpaceMap k a -> k -> Maybe a
lookupInnermostScope nsm@(NsMap _gs localDefs) ident  =
    case localDefs of
        (ls : _lss) -> Prelude.lookup ident ls
        [] -> lookupGlobal nsm ident

-- | flatten a namespace into a assoc list
--
--  @nameSpaceToList ns = (localDefInnermost ns ++ .. ++ localDefsOutermost ns) ++ globalDefs ns@
nsMapToList :: (Ord k) => NameSpaceMap k a -> [(k, a)]
nsMapToList (NsMap gs lss)  = concat lss ++ Map.toList gs

-- | Merge two namespaces. If they disagree on the types of any
--   variables, all bets are off.
mergeNameSpace :: (Ord k) =>
                  NameSpaceMap k a
               -> NameSpaceMap k a
               -> NameSpaceMap k a
mergeNameSpace (NsMap global1 local1) (NsMap global2 local2) =
  NsMap (Map.union global1 global2) (localUnion local1 local2)
  where localUnion (l1:ls1) (l2:ls2) =
          List.unionBy (\p1 p2 -> fst p1 == fst p2) l1 l2 : localUnion ls1 ls2
        localUnion [] ls2 = ls2
        localUnion ls1 [] = ls1
