{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Parser
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Language.C parser
-----------------------------------------------------------------------------
module Language.C.Parser (
    -- * Simple API
    parseC,
    -- * Parser Monad
    P,execParser,execParser_,builtinTypeNames,
    -- * Exposed Parsers
    translUnitP, extDeclP, statementP, expressionP,
    -- * Parser Monad
    ParseError(..)
)
where
import Language.C.Parser.Parser (parseC,translUnitP, extDeclP, statementP, expressionP)
import Language.C.Parser.ParserMonad (execParser, ParseError(..),P)
import Language.C.Parser.Builtin (builtinTypeNames)

import Language.C.Data

-- | run the given parser using a new name supply and builtin typedefs
--   see 'execParser'
--
-- Synopsis: @runParser parser inputStream initialPos@
execParser_ :: P a -> InputStream -> Position -> Either ParseError a
execParser_ parser input pos =
  fmap fst $ execParser parser input pos builtinTypeNames newNameSupply
