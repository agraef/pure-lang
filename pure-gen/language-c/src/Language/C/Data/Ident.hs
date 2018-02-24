{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data.Ident
-- Copyright   :  (c) [1995..1999] Manuel M. T. Chakravarty
--                (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- This module provides the notion of identifiers in C, speed up using hashing.
-- Identifiers are associated optionally associated with a 'NodeInfo', i.e. with
-- a unique 'Name' and a source location ('Position'). The ordering relation on
-- identifiers is based on the hash and does not follow the lexical order.
-----------------------------------------------------------------------------
module Language.C.Data.Ident (
    Ident(..),
    SUERef(..), isAnonymousRef,
    mkIdent, builtinIdent, internalIdent, internalIdentAt, isInternalIdent, identToString, dumpIdent)
where

-- TODO (comment from manuel):
--  * Hashing is not 8bit clean.

import Data.Char
import Language.C.Data.Position
import Language.C.Data.Node
import Language.C.Data.Name   (Name,nameId)
import Data.Generics

-- | References uniquely determining a struct, union or enum type.
-- Those are either identified by an string identifier, or by a unique
-- name (anonymous types).
data SUERef =  AnonymousRef Name
             | NamedRef Ident
    deriving (Typeable, Data, Ord, Eq, Show) --, Read

-- | Return true if the struct\/union\/enum reference is anonymous.
isAnonymousRef :: SUERef -> Bool
isAnonymousRef (AnonymousRef _) = True
isAnonymousRef _ = False

-- | C identifiers
data Ident = Ident String       -- lexeme
                   {-# UNPACK #-}   !Int     -- hash to speed up equality check
                   NodeInfo                   -- attributes of this ident. incl. position
             deriving (Data,Typeable,Show) -- Read

-- the definition of the equality allows identifiers to be equal that are
-- defined at different source text positions, and aims at speeding up the
-- equality test, by comparing the lexemes only if the two numbers are equal
--
instance Eq Ident where
  (Ident s h _) == (Ident s' h' _) = (h == h') && (s == s')

-- this does *not* follow the alphanumerical ordering of the lexemes
--
instance Ord Ident where
  compare (Ident s h _) (Ident s' h' _) = compare (h, s) (h', s')

-- identifiers are attributed
instance CNode Ident where
  nodeInfo (Ident _ _ at) = at
instance Pos Ident where
  posOf = posOfNode . nodeInfo
-- to speed up the equality test we compute some hash-like value for each
-- identifiers lexeme and store it in the identifiers representation

-- hash function from the dragon book pp437; assumes 7 bit characters and needs
-- the (nearly) full range of values guaranteed for `Int' by the Haskell
-- language definition; can handle 8 bit characters provided we have 29 bit
-- for the `Int's without sign
--
quad                 :: String -> Int
quad (c1:c2:c3:c4:s)  = ((ord c4 * bits21
                          + ord c3 * bits14
                          + ord c2 * bits7
                          + ord c1)
                         `mod` bits28)
                        + (quad s `mod` bits28)
quad (c1:c2:c3:[]  )  = ord c3 * bits14 + ord c2 * bits7 + ord c1
quad (c1:c2:[]     )  = ord c2 * bits7 + ord c1
quad (c1:[]        )  = ord c1
quad ([]           )  = 0

bits7 :: Int
bits7  = 2^(7::Int)
bits14 :: Int
bits14 = 2^(14::Int)
bits21 :: Int
bits21 = 2^(21::Int)
bits28 :: Int
bits28 = 2^(28::Int)

-- | build an identifier from a string.
--
-- * only minimal error checking, e.g., the characters of the identifier are
--   not checked for being alphanumerical only; the correct lexis of the
--   identifier should be ensured by the caller, e.g., the scanner.
--
-- * for reasons of simplicity the complete lexeme is hashed.
mkIdent            :: Position -> String -> Name -> Ident
mkIdent pos s name  = Ident s (quad s) (mkNodeInfo' pos (pos,length s) name)

-- | returns an /internal/ identifier (has internal position and no unique name)
internalIdent   :: String -> Ident
internalIdent s  = Ident s (quad s) (mkNodeInfoOnlyPos internalPos)

-- | return an /internal/ identifier with position info
internalIdentAt :: Position -> String -> Ident
internalIdentAt pos s = Ident s (quad s) (mkNodeInfoPosLen pos (pos, length s))

-- | returns a /builtin/ identifier (has builtin position and no unique name)
builtinIdent   :: String -> Ident
builtinIdent s  = Ident s (quad s) (mkNodeInfoOnlyPos builtinPos)

-- | return @True@ if the given identifier is /internal/
isInternalIdent :: Ident -> Bool
isInternalIdent (Ident _ _ nodeinfo) = isInternalPos (posOfNode nodeinfo)

-- | string of an identifier
identToString               :: Ident -> String
identToString (Ident s _ _)  = s

-- | dump the identifier string and its positions for debugging purposes
dumpIdent     :: Ident -> String
dumpIdent ide  = identToString ide ++ " at " ++ show (nodeInfo ide)
