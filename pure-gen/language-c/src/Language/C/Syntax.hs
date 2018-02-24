-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Syntax of C files: The abstract syntax tree and constants.
-----------------------------------------------------------------------------
module Language.C.Syntax (
     -- * Constants
     module Language.C.Syntax.Constants,
     -- * Syntax tree
     module Language.C.Syntax.AST,
)
where
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
