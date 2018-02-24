-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data.RList
-- Copyright   :  (c) [2007..2008] Duncan Coutts, Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Due to the way the grammar is constructed we very often have to build lists
-- in reverse. To make sure we do this consistently and correctly we have a
-- newtype to wrap the reversed style of list:
-----------------------------------------------------------------------------
module Language.C.Data.RList (
    RList,Reversed(..),
    empty,singleton,snoc,rappend,appendr,rappendr,rmap,reverse,
    viewr,
)
where
import Prelude hiding (reverse)
import qualified Data.List as List

newtype Reversed a = Reversed a
type RList a = Reversed [a]
empty :: Reversed [a]
empty = Reversed []

singleton :: a -> Reversed [a]
singleton x = Reversed [x]

snoc :: Reversed [a] -> a -> Reversed [a]
snoc (Reversed xs) x = Reversed (x : xs)
infixr 5 `snoc`

rappend :: Reversed [a] -> [a] -> Reversed [a]
rappend (Reversed xs) ys = Reversed (List.reverse ys ++ xs)

appendr :: [a] -> Reversed [a] -> Reversed [a]
appendr xs (Reversed ys) = Reversed (ys ++ List.reverse xs)

rappendr :: Reversed [a] -> Reversed [a] -> Reversed [a]
rappendr (Reversed xs) (Reversed ys) = Reversed (ys ++ xs)

rmap :: (a -> b) -> Reversed [a] -> Reversed [b]
rmap f (Reversed xs) = Reversed (map f xs)

reverse :: Reversed [a] -> [a]
reverse (Reversed xs) = List.reverse xs

viewr :: Reversed [a] -> (Reversed [a] , a)
viewr (Reversed []) = error "viewr: empty RList"
viewr (Reversed (x:xs)) = (Reversed xs, x)
