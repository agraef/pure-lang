{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax.Constants
-- Copyright   :  (c) 2007..2008 Duncan Coutts, Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- This module provides support for representing, checking and exporting c
-- constants, i.e. integral, float, character and string constants.
-----------------------------------------------------------------------------
module Language.C.Syntax.Constants (
  -- * Utilities
  escapeChar, unescapeChar, unescapeString,
  Flags(..), noFlags, setFlag, clearFlag, testFlag,
  -- * C char constants (and multi-character character constants)
  cChar, cChar_w, cChars, CChar(..), getCChar, getCCharAsInt, isWideChar, showCharConst,
  -- * C integral constants
  CIntFlag(..), CIntRepr(..), cInteger, CInteger(..), getCInteger,readCInteger,
  -- * C floating point constants
  cFloat,  CFloat(..), readCFloat,
  -- * C string literals
  cString, cString_w, CString(..), getCString, showStringLit, concatCStrings,
)
where
import Data.Bits
import Data.Char
import Numeric (showOct, showHex, readHex, readOct, readDec)
import Language.C.Data.Node
import Language.C.Data.Position
import Data.Generics

-- | C char constants (abstract)
data CChar = CChar
              !Char
              !Bool  -- wide flag
           | CChars
              [Char] -- multi-character character constant
              !Bool   -- wide flag
           deriving (Eq,Ord,Data,Typeable)

instance Show CChar where
    showsPrec _ (CChar c wideflag)   = _showWideFlag wideflag . showCharConst c
    showsPrec _ (CChars cs wideflag) = _showWideFlag wideflag . (sQuote $ concatMap escapeCChar cs)

-- | @showCharConst c@ prepends _a_ String representing the C char constant corresponding to @c@.
-- If necessary uses octal or hexadecimal escape sequences.
showCharConst :: Char -> ShowS
showCharConst c = sQuote $ escapeCChar c

_showWideFlag :: Bool -> ShowS
_showWideFlag flag = if flag then showString "L" else id

-- | get the haskell representation of a char constant
getCChar :: CChar -> [Char]
getCChar (CChar  c _)   = [c]
getCChar (CChars  cs _) = cs

-- | get integer value of a C char constant
-- undefined result for multi-char char constants
getCCharAsInt :: CChar -> Integer
getCCharAsInt (CChar c _) = fromIntegral (fromEnum c)
getCCharAsInt (CChars _cs _) = error "integer value of multi-character character constants is implementation defined"

-- | return @true@ if the character constant is /wide/.
isWideChar :: CChar -> Bool
isWideChar (CChar _ wideFlag) = wideFlag
isWideChar (CChars _ wideFlag) = wideFlag

-- | construct a character constant from a haskell 'Char'
-- Use 'cchar_w' if you want a wide character constant.
cChar :: Char -> CChar
cChar c = CChar c False

-- | construct a wide chararacter constant
cChar_w :: Char -> CChar
cChar_w c = CChar c True

-- | create a multi-character character constant
cChars :: [Char] -> Bool -> CChar
cChars = CChars

-- | datatype for memorizing the representation of an integer
data CIntRepr = DecRepr | HexRepr | OctalRepr deriving (Eq,Ord,Enum,Bounded,Data,Typeable)

-- | datatype representing type flags for integers
data CIntFlag = FlagUnsigned | FlagLong | FlagLongLong | FlagImag deriving (Eq,Ord,Enum,Bounded,Data,Typeable)
instance Show CIntFlag where
    show FlagUnsigned = "u"
    show FlagLong = "L"
    show FlagLongLong = "LL"
    show FlagImag = "i"

{-# SPECIALIZE setFlag :: CIntFlag -> Flags CIntFlag -> Flags CIntFlag #-}
{-# SPECIALIZE clearFlag :: CIntFlag -> Flags CIntFlag -> Flags CIntFlag #-}
{-# SPECIALIZE testFlag :: CIntFlag -> Flags CIntFlag -> Bool #-}

data CInteger = CInteger
                 !Integer
                 !CIntRepr
                 !(Flags CIntFlag)  -- integer flags
                 deriving (Eq,Ord,Data,Typeable)
instance Show CInteger where
    showsPrec _ (CInteger i repr flags) = showInt i . showString (concatMap showIFlag [FlagUnsigned .. ]) where
        showIFlag f = if testFlag f flags then show f else []
        showInt i = case repr of DecRepr -> shows i
                                 OctalRepr -> showString "0" . showOct i
                                 HexRepr -> showString "0x" . showHex i

-- To be used in the lexer
-- Note that the flag lexer won't scale
readCInteger :: CIntRepr -> String -> Either String CInteger
readCInteger repr str =
  case readNum str of
    [(n,suffix)] -> mkCInt n suffix
    parseFailed  -> Left $ "Bad Integer literal: "++show parseFailed
  where
    readNum = case repr of DecRepr -> readDec; HexRepr -> readHex; OctalRepr -> readOct
    mkCInt n suffix = either Left (Right . CInteger n repr)  $ readSuffix suffix
    readSuffix = parseFlags noFlags
    parseFlags flags [] = Right flags
    parseFlags flags ('l':'l':fs) = parseFlags (setFlag FlagLongLong flags) fs
    parseFlags flags ('L':'L':fs) = parseFlags (setFlag FlagLongLong flags) fs
    parseFlags flags (f:fs) =
      let go1 flag = parseFlags (setFlag flag flags) fs in
      case f of
        'l' -> go1 FlagLong ; 'L' -> go1 FlagLong
        'u' -> go1 FlagUnsigned ; 'U' -> go1 FlagUnsigned
        'i' -> go1 FlagImag ; 'I' -> go1 FlagImag; 'j' -> go1 FlagImag; 'J' -> go1 FlagImag
        _ -> Left $ "Unexpected flag " ++ show f

getCInteger :: CInteger -> Integer
getCInteger (CInteger i _ _) = i

-- | construct a integer constant (without type flags) from a haskell integer
cInteger :: Integer -> CInteger
cInteger i = CInteger i DecRepr noFlags


-- | Floats (represented as strings)
data CFloat = CFloat
               !String
               deriving (Eq,Ord,Data,Typeable)
instance Show CFloat where
  showsPrec _ (CFloat internal) = showString internal

cFloat :: Float -> CFloat
cFloat = CFloat . show

-- dummy implementation
readCFloat :: String -> CFloat
readCFloat = CFloat

-- | C String literals
data CString = CString
                [Char]    -- characters
                Bool      -- wide flag
                deriving (Eq,Ord,Data,Typeable)
instance Show CString where
    showsPrec _ (CString str wideflag) = _showWideFlag wideflag . showStringLit str

-- construction
cString :: String -> CString
cString str = CString str False
cString_w :: String -> CString
cString_w str = CString str True

-- selectors
getCString :: CString -> String
getCString (CString str _) = str
isWideString :: CString -> Bool
isWideString (CString _ wideflag) = wideflag

-- | concatenate a list of C string literals
concatCStrings :: [CString] -> CString
concatCStrings cs = CString (concatMap getCString cs) (any isWideString cs)

-- | @showStringLiteral s@ prepends a String representing the C string literal corresponding to @s@.
-- If necessary it uses octal or hexadecimal escape sequences.
showStringLit :: String -> ShowS
showStringLit = dQuote . concatMap showStringChar
  where
  showStringChar c | isSChar c = return c
                     | c == '"'  = "\\\""
                     | otherwise = escapeChar c



-- | @isAsciiSourceChar b@ returns @True@ if the given character is a character which
--   may appear in a ASCII C source file and is printable.
isAsciiSourceChar :: Char -> Bool
isAsciiSourceChar c = isAscii c && isPrint c

-- | @isCChar c@ returns true, if c is a source character which does not have to be escaped in
--   C char constants (C99: 6.4.4.4)
isCChar :: Char -> Bool
isCChar '\\' = False
isCChar '\'' = False
isCChar '\n' = False
isCChar c = isAsciiSourceChar c

-- | @escapeCChar c@ escapes c for use in a char constant
escapeCChar :: Char -> String
escapeCChar '\'' = "\\'"
escapeCChar c | isCChar c = [c]
              | otherwise = escapeChar c

-- | @isSChar c@ returns true if c is a source character which does not have to be escaped in C string
--  literals (C99: 6.4.5)
isSChar :: Char -> Bool
isSChar '\\' = False
isSChar '\"' = False
isSChar '\n' = False
isSChar c = isAsciiSourceChar c

showOct' :: Int -> String
showOct' i = replicate (3 - length s) '0' ++ s
  where s = showOct i ""

escapeChar :: Char -> String
escapeChar '\\' = "\\\\"
escapeChar '\a' = "\\a"
escapeChar '\b' = "\\b"
escapeChar '\ESC' = "\\e";
escapeChar '\f' = "\\f"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar '\v' = "\\v"
escapeChar c  | (ord c) < 512   = '\\' : showOct' (ord c)
              | otherwise       = '\\' : 'x'  : showHex (ord c) ""

unescapeChar :: String -> (Char, String)
unescapeChar ('\\':c:cs)  = case c of
       'n'  -> ('\n', cs)
       't'  -> ('\t', cs)
       'v'  -> ('\v', cs)
       'b'  -> ('\b', cs)
       'r'  -> ('\r', cs)
       'f'  -> ('\f', cs)
       'a'  -> ('\a', cs)
       'e'  -> ('\ESC', cs)  -- GNU extension
       'E'  -> ('\ESC', cs)  -- GNU extension
       '\\' -> ('\\', cs)
       '?'  -> ('?', cs)
       '\'' -> ('\'', cs)
       '"'  -> ('"', cs)
       'x'  -> case head' "bad escape sequence" (readHex cs) of
                 (i, cs') -> (toEnum i, cs')
       _    -> case head' "bad escape sequence" (readOct' (c:cs)) of
                 (i, cs') -> (toEnum i, cs')
unescapeChar (c   :cs)    = (c, cs)
unescapeChar []  = error $ "unescape char: empty string"

readOct' :: ReadS Int
readOct' s = map (\(i, cs) -> (i, cs ++ rest)) (readOct octStr)
  where octStr = takeWhile isOctDigit $ take 3 s
        rest = drop (length octStr) s

unescapeString :: String -> String
unescapeString [] = []
unescapeString cs = case unescapeChar cs of
                        (c, cs') -> c : unescapeString cs'

-- helpers
sQuote :: String -> ShowS
sQuote s t = "'" ++ s ++ "'" ++ t
dQuote :: String -> ShowS
dQuote s t = ('"' : s) ++ "\"" ++ t
head' :: String -> [a] -> a
head' err []  = error err
head' _ (x:_) = x

-- TODO: Move to separate file ?
newtype Flags f = Flags Integer deriving (Eq,Ord,Data,Typeable)
noFlags :: Flags f
noFlags = Flags 0
setFlag :: (Enum f) => f -> Flags f -> Flags f
setFlag flag (Flags k)   = Flags$ k  `setBit` fromEnum flag
clearFlag :: (Enum f) => f -> Flags f -> Flags f
clearFlag flag (Flags k) = Flags$ k `clearBit` fromEnum flag
testFlag :: (Enum f) => f -> Flags f -> Bool
testFlag flag (Flags k)  = k `testBit` fromEnum flag
