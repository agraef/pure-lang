{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data.Position
-- Copyright   :  (c) [1995..2000] Manuel M. T. Chakravarty
--                    [2008..2009] Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- Source code position
-----------------------------------------------------------------------------
module Language.C.Data.Position (
  --
  -- source text positions
  --
  Position(),
  position,
  PosLength,
  posFile,posRow,posColumn,posOffset,
  initPos, isSourcePos,
  nopos, isNoPos,
  builtinPos, isBuiltinPos,
  internalPos, isInternalPos,
  incPos, retPos, adjustPos,
  incOffset,
  Pos(..),
) where
import Data.Generics

-- | uniform representation of source file positions
data Position = Position { posOffset :: {-# UNPACK #-} !Int  -- ^ absolute offset in the preprocessed file
                         , posFile :: String                 -- ^ source file
                         , posRow :: {-# UNPACK #-} !Int     -- ^ row (line)  in the original file. Affected by #LINE pragmas.
                         , posColumn :: {-# UNPACK #-} !Int  -- ^ column in the preprocessed file. Inaccurate w.r.t. to the original
                                                             --   file in the presence of preprocessor macros.
                         }
              | NoPosition
              | BuiltinPosition
              | InternalPosition
                deriving (Eq, Ord, Typeable, Data)

-- | Position and length of a token
type PosLength = (Position,Int)

instance Show Position where
  show (Position _ fname row _) = "(" ++ show fname ++ ": line " ++ show row ++ ")"
  show NoPosition               = "<no file>"
  show BuiltinPosition          = "<builtin>"
  show InternalPosition         = "<internal>"

{-# DEPRECATED posColumn "column number information is inaccurate in presence of macros - do not rely on it." #-}

-- | @position absoluteOffset fileName lineNumber columnNumber@ initializes a @Position@ using the given arguments
position :: Int -> String -> Int -> Int -> Position
position = Position

-- | class of type which aggregate a source code location
class Pos a where
    posOf :: a -> Position

-- | initialize a Position to the start of the translation unit starting in the given file
initPos :: FilePath -> Position
initPos file = Position 0 file 1 1

-- | returns @True@ if the given position refers to an actual source file
isSourcePos :: Position -> Bool
isSourcePos (Position _ _ _ _) = True
isSourcePos _                  = False

-- | no position (for unknown position information)
nopos :: Position
nopos  = NoPosition

-- | returns @True@ if the there is no position information available
isNoPos :: Position -> Bool
isNoPos NoPosition = True
isNoPos _          = False

-- | position attached to built-in objects
--
builtinPos :: Position
builtinPos  = BuiltinPosition

-- | returns @True@ if the given position refers to a builtin definition
isBuiltinPos :: Position -> Bool
isBuiltinPos BuiltinPosition = True
isBuiltinPos _               = False

-- | position used for internal errors
internalPos :: Position
internalPos = InternalPosition

-- | returns @True@ if the given position is internal
isInternalPos :: Position -> Bool
isInternalPos InternalPosition = True
isInternalPos _                = False

{-# INLINE incPos #-}
-- | advance column
incPos :: Position -> Int -> Position
incPos (Position offs fname row col) n = Position (offs + n) fname row (col + n)
incPos p _                             = p

{-# INLINE retPos #-}
-- | advance to next line
retPos :: Position -> Position
retPos (Position offs fname row _) = Position (offs+1) fname (row + 1) 1
retPos p                           = p

{-# INLINE adjustPos #-}
-- | adjust position: change file and line number, reseting column to 1. This is usually
--   used for #LINE pragmas. The absolute offset is not changed - this can be done
--   by @adjustPos newFile line . incPos (length pragma)@.
adjustPos :: FilePath -> Int -> Position -> Position
adjustPos fname row (Position offs _ _ _) = Position offs fname row 1
adjustPos _ _ p                           = p

{-# INLINE incOffset #-}
-- | advance just the offset
incOffset :: Position -> Int -> Position
incOffset (Position o f r c) n = Position (o + n) f r c
incOffset p n                  = p
