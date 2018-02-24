-- |
-- Module      :  Language.C.Parser.Builtin
-- Copyright   :  (c) 2001 Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- This module provides information about builtin entities.
--
--  Currently, only builtin type names are supported.  The only builtin type
--  name is `__builtin_va_list', which is a builtin of GNU C.
--
module Language.C.Parser.Builtin (
  builtinTypeNames
) where
import Language.C.Data.Ident (Ident, builtinIdent)

-- predefined type names
--
builtinTypeNames :: [Ident]
builtinTypeNames  = [builtinIdent "__builtin_va_list"]
