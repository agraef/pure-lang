-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Common data types for Language.C: Identifiers, unique names, source code locations,
-- ast node attributes and extensible errors.
-----------------------------------------------------------------------------
module Language.C.Data (
     -- * Input stream
     module Language.C.Data.InputStream,
     -- * Identifiers
     SUERef(..), isAnonymousRef,
     Ident,mkIdent, identToString, internalIdent, isInternalIdent, builtinIdent,
     -- * Unqiue names
     Name(..),newNameSupply,
     -- * Source code positions
     Position(..),Pos(..),
     initPos, nopos,builtinPos,internalPos,
     isSourcePos,isBuiltinPos,isInternalPos,
     -- * Syntax tree nodes
     NodeInfo(..),CNode(..),
     fileOfNode,posOfNode,nameOfNode,
     undefNode,mkNodeInfoOnlyPos,mkNodeInfo,
     internalNode, -- DEPRECATED
     -- * Extensible errors
     module Language.C.Data.Error
)
where
import Language.C.Data.InputStream
import Language.C.Data.Ident
import Language.C.Data.Name
import Language.C.Data.Position
import Language.C.Data.Error
import Language.C.Data.Node
