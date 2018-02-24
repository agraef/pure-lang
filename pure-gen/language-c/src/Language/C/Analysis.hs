-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  ghc
--
-- Analysis of the AST.
--
-- Currently, we provide a monad for analysis and analyze declarations and types.
-- Especially note that there is no direct support for analyzing function bodies and
-- constant expressions.
--
-- /NOTE/ This is an experimental interface, and therefore the API will change in the
-- future.
--
-- DONE:
--
--  * Name analysis framework
--
--  * File-scope analysis
--
--  * Declaration analysis
--
-- TODO:
--
--  * Type checking expressions
--
--  * Constant expression evaluation (CEE)
--
--  * Typed representation of attributes (depends on CEE)
--
--  * Normalized representation of initializers
--
--  * Support for analyzing function bodies (depends on CEE)
--
--  * Normalizing expressions and statements
--
--  * Formal rules how to link back to the AST using NodeInfo fields
--
--  * Typed assembler representation

-----------------------------------------------------------------------------
module Language.C.Analysis (
    -- * Semantic representation
    module Language.C.Analysis.SemRep,
    -- * Error datatypes for the analysis
    module Language.C.Analysis.SemError,
    -- * Traversal monad
    module Language.C.Analysis.TravMonad,
    -- * Top level analysis
    module Language.C.Analysis.AstAnalysis,
    -- * Analyzing declarations
    module Language.C.Analysis.DeclAnalysis,
    -- * Debug print
    module Language.C.Analysis.Debug,
)
where
import Language.C.Analysis.SemError
import Language.C.Analysis.SemRep

import Language.C.Analysis.DefTable
import Language.C.Analysis.TravMonad

import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.DeclAnalysis

import Language.C.Analysis.Debug
