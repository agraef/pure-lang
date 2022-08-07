-----------------------------------------------------------------------------
-- Module      :  Lexer.x
-- Copyright   : (c) [1999..2004] Manuel M T Chakravarty
--               (c) 2005 Duncan Coutts
--               (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
--  Lexer for C files, after being processed by the C preprocessor
--
--  We assume that the input already went through cpp.  Thus, we do not handle
--  comments and preprocessor directives here.  It supports the
--  C99 `restrict' extension: <http://www.lysator.liu.se/c/restrict.html> as
--  well as inline functions.
--
--  Comments:
--
--  * Universal character names and multi-character character constants,
--    as well as trigraphs are unsupported. They are lexed, but yield an error.
--
--  * We add `typedef-name' (K&R 8.9) as a token, as proposed in K&R A13.
--    However, as these tokens cannot be recognized lexically, but require a
--    context analysis, they are never produced by the lexer, but instead have
--    to be introduced in a later phase (by converting the corresponding
--    identifiers).
--
--  * We also recognize GNU C `__attribute__', `__extension__', `__complex__',
--    `__const',  `__const__', `__imag', `__imag__', `__inline', `__inline__',
--    `__real', `__real__, `__restrict', and `__restrict__'.
--
--  * Any line starting with `#pragma' is ignored.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988.
--
--  With C99 we refer to ``ISO/IEC 9899:TC3'',
--  available online at http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf.
--
--- TODO ----------------------------------------------------------------------
--
--  * There are more GNU C specific keywords.  Add them and change `Parser.y'
--    correspondingly (in particular, most tokens within __attribute ((...))
--    expressions are actually keywords, but we handle them as identifiers at
--    the moment).
--
--  * Add support for bytestrings

{

module Language.C.Parser.Lexer (lexC, parseError) where

import Data.Char (chr, isDigit)
import Data.Word (Word8)
import Control.Monad (liftM, when)

import Language.C.Data.InputStream
  (InputStream, inputStreamEmpty, takeByte, takeChar, takeChars)

-- (    InputStream, readInputStream,inputStreamToString,inputStreamFromString,
--     takeByte, takeChar, inputStreamEmpty, takeChars,
--     countLines,
-- )
import Language.C.Data.Position
import Language.C.Data.Ident    (mkIdent)

import Language.C.Syntax.Constants

import Language.C.Parser.Tokens
import Language.C.Parser.ParserMonad
}

$space = [ \ \t ]                           -- horizontal white space
$eol   = \n                                 -- end of line

$letter   = [a-zA-Z]
$identletter = [a-zA-Z_\$]                  -- GNU extension: allow $ in variable names
$octdigit = 0-7
$digit    = 0-9
$digitNZ  = 1-9
$hexdigit = [0-9a-fA-F]

$inchar   = . # [ \\ \' \n \r ]       -- valid character in char constant
$instr    = . # [ \\ \" \n \r ]       -- valid character in a string literal
$infname  = . # [ \\ \" ]             -- valid character in a filename

@sp  = $space*

-- character escape sequence (follows K&R A2.5.2)
--
-- * also used for strings
-- * C99: 6.4.4.4
@charesc  = \\([ntvbrfaeE\\\?\'\"]|$octdigit{1,3}|x$hexdigit+)
@ucn      = \\u$hexdigit{4}|\\U$hexdigit{8}

-- components of integer constants
--
-- * C99: 6.4.4.1
@int = $digitNZ$digit*

-- integer suffixes
@llsuffix  = ll|LL
@gnusuffix = [ij]?
@intsuffix = [uU][lL]?|[uU]@llsuffix|[lL][uU]?|@llsuffix[uU]?
@intgnusuffix = @intsuffix@gnusuffix?|@gnusuffix@intsuffix?

-- components of float constants (follows K&R A2.5.3)
--
-- * C99: 6.4.4.2
@digits    = $digit+
@intpart   = @digits
@fractpart = @digits

@mantpart  = @intpart?\.@fractpart|@intpart\.
@exppart   = [eE][\+\-]?@digits

@hexprefix = 0x
@hexdigits = $hexdigit+
@hexmant   = @hexdigits?\.@hexdigits|@hexdigits\.
@binexp    = [pP][\+\-]?@digits

-- Suffixes `qQwW` are GNU floating type extensions: <https://gcc.gnu.org/onlinedocs/gcc/Floating-Types.html>
@floatsuffix    = [fFlLqQwW]
@floatgnusuffix = @floatsuffix@gnusuffix?|@gnusuffix@floatsuffix?



tokens :-

-- whitespace (follows K&R A2.1)
--
-- * horizontal and vertical tabs, newlines, and form feeds are filter out by
--   `Lexers.ctrlLexer'
--
-- * comments are not handled, as we assume the input already went through cpp
--
$white+         ;

-- #line directive (K&R A12.6)
--
-- * allows further ints after the file name a la GCC; as the GCC CPP docu
--   doesn't say how many ints there can be, we allow an unbound number
--
\#$space*@digits$space*(\"($infname|@charesc)*\"$space*)?(@int$space*)*\r?$eol
  { \pos len str -> setPos (adjustLineDirective len (takeChars len str) pos) >> lexToken' False }

-- #pragma directive (K&R A12.8)
--
-- * we simply ignore any #pragma (but take care to update the position
--   information)
--
\#$space*pragma.*$eol   ;

-- #ident directive, eg used by rcs/cvs
--
-- * we simply ignore any #ident (but take care to update the position
--   information)
--
\#$space*ident.*$eol    ;

-- identifiers and keywords (follows K&R A2.3 and A2.4)
--
$identletter($identletter|$digit)*  { \pos len str -> idkwtok (takeChars len str) pos }

-- constants (follows K&R A2.5)
--
-- * K&R,C99 explicitly mention `enumeration-constants'; however, as they are
--   lexically identifiers, we do not have an extra case for them
--

-- integer constants (follows K&R A2.5.1, C99 6.4.4.1)
-- NOTE: 0 is lexed as octal integer constant, and readCOctal takes care of this
0$octdigit*@intgnusuffix?       { token_plus CTokILit readCOctal }
$digitNZ$digit*@intgnusuffix?   { token_plus CTokILit (readCInteger DecRepr) }
0[xX]$hexdigit+@intgnusuffix?   { token_plus CTokILit (readCInteger HexRepr . drop 2) }

(0$octdigit*|$digitNZ$digit*|0[xX]$hexdigit+)[uUlL]+ { token_fail "Invalid integer constant suffix" }

-- character constants (follows K&R A2.5.2, C99 6.4.4.4)
--
-- * Universal Character Names are unsupported and cause an error.
\'($inchar|@charesc)\'  { token CTokCLit (cChar . fst . unescapeChar . tail) }
L\'($inchar|@charesc)\' { token CTokCLit (cChar_w . fst . unescapeChar . tail . tail) }
\'($inchar|@charesc){2,}\' { token CTokCLit (flip cChars False . unescapeMultiChars .tail) }
L\'($inchar|@charesc){2,}\' { token CTokCLit (flip cChars True . unescapeMultiChars . tail . tail) }

-- float constants (follows K&R A2.5.3. C99 6.4.4.2)
--
-- * NOTE: Hexadecimal floating constants without binary exponents are forbidden.
--         They generate a lexer error, because they are hard to recognize in the parser.
(@mantpart@exppart?|@intpart@exppart)@floatgnusuffix?  { token CTokFLit readCFloat }
@hexprefix(@hexmant|@hexdigits)@binexp@floatgnusuffix? { token CTokFLit readCFloat }
@hexprefix@hexmant                                     { token_fail "Hexadecimal floating constant requires an exponent" }

-- string literal (follows K&R A2.6)
-- C99: 6.4.5.
\"($instr|@charesc)*\"      { token CTokSLit (cString . unescapeString . init . tail) }
L\"($instr|@charesc)*\"     { token CTokSLit (cString_w . unescapeString . init . tail . tail) }

L?\'@ucn\'                        { token_fail "Universal character names are unsupported" }
L?\'\\[^0-7'\"\?\\abfnrtvuUx]\'     { token_fail "Invalid escape sequence" }
L?\"($inchar|@charesc)*@ucn($inchar|@charesc|@ucn)*\" { token_fail "Universal character names in string literals are unsupported"}

-- operators and separators
--
"("   { token_ 1 CTokLParen }
")"   { token_ 1 CTokRParen  }
"["   { token_ 1 CTokLBracket }
"]"   { token_ 1 CTokRBracket }
"->"  { token_ 2 CTokArrow }
"."   { token_ 1 CTokDot }
"!"   { token_ 1 CTokExclam }
"~"   { token_ 1 CTokTilde }
"++"  { token_ 2 CTokInc }
"--"  { token_ 2 CTokDec }
"+"   { token_ 1 CTokPlus }
"-"   { token_ 1 CTokMinus }
"*"   { token_ 1 CTokStar }
"/"   { token_ 1 CTokSlash }
"%"   { token_ 1 CTokPercent }
"&"   { token_ 1 CTokAmper }
"<<"  { token_ 2 CTokShiftL }
">>"  { token_ 2 CTokShiftR }
"<"   { token_ 1 CTokLess }
"<="  { token_ 2 CTokLessEq }
">"   { token_ 1 CTokHigh }
">="  { token_ 2 CTokHighEq }
"=="  { token_ 2 CTokEqual }
"!="  { token_ 2 CTokUnequal }
"^"   { token_ 1 CTokHat }
"|"   { token_ 1 CTokBar }
"&&"  { token_ 2 CTokAnd }
"||"  { token_ 2 CTokOr }
"?"   { token_ 1 CTokQuest }
":"   { token_ 1 CTokColon }
"="   { token_ 1 CTokAssign }
"+="  { token_ 2 CTokPlusAss }
"-="  { token_ 2 CTokMinusAss }
"*="  { token_ 2 CTokStarAss }
"/="  { token_ 2 CTokSlashAss }
"%="  { token_ 2 CTokPercAss }
"&="  { token_ 2 CTokAmpAss }
"^="  { token_ 2 CTokHatAss }
"|="  { token_ 2 CTokBarAss }
"<<=" { token_ 3 CTokSLAss }
">>=" { token_ 3 CTokSRAss }
","   { token_ 1 CTokComma }
\;    { token_ 1 CTokSemic }
"{"   { token_ 1 CTokLBrace }
"}"   { token_ 1 CTokRBrace }
"..." { token_ 3 CTokEllipsis }


{
-- Fix the 'octal' lexing of '0'
readCOctal :: String -> Either String CInteger
readCOctal s@('0':r) =
    case r of
        (c:_) | isDigit c -> readCInteger OctalRepr r
        _                 -> readCInteger DecRepr s

-- We use the odd looking list of string patterns here rather than normal
-- string literals since GHC converts the latter into a sequence of string
-- comparisons (ie a linear search) but it translates the former using its
-- effecient pattern matching which gives us the expected radix-style search.
-- This change makes a significant performance difference [chak]
--
-- To make this a little more maintainable, we autogenerate it from this list,
-- using the script GenerateKeywordMatch.hs (in /src)
{-
alignof @__, asm @__, auto
break, bool _Bool,
case, char, const @__, continue, complex _Complex __complex__
default, do, double,
else, enum, extern,
float, for, goto,
if, inline @__, int, __int128, long,
register, restrict @__, return
short, signed @__, sizeof, static, struct, switch,
typedef, typeof @__, thread __thread,
union, unsigned, void, volatile @__,
while,
label __label__
(CTokGnuC GnuCAttrTok) __attribute __attribute__
(CTokGnuC GnuCExtTok) __extension__
(CTokGnuC GnuCComplexReal) __real __real__
(CTokGnuC GnuCComplexImag) __imag __imag__
(CTokGnuC GnuCVaArg) __builtin_va_arg
(CTokGnuC GnuCOffsetof) __builtin_offsetof
(CTokGnuC GnuCTyCompat) __builtin_types_compatible_p
-}
-- Tokens: alignof __alignof __alignof__ asm __asm __asm__ __attribute __attribute__ auto _Bool break __builtin_offsetof __builtin_types_compatible_p __builtin_va_arg case char _Complex __complex__ const __const __const__ continue default do double else enum __extension__ extern float __float128 _Float128 for goto if __imag __imag__ inline __inline __inline__ int __int128 __label__ long __real __real__ register __restrict __restrict__ return short signed __signed __signed__ sizeof static struct switch __thread typedef typeof __typeof __typeof__ union unsigned void volatile __volatile __volatile__ while
idkwtok ('_' : 'B' : 'o' : 'o' : 'l' : []) = tok 5 CTokBool
idkwtok ('_' : 'C' : 'o' : 'm' : 'p' : 'l' : 'e' : 'x' : []) = tok 8 CTokComplex
idkwtok ('_' : '_' : 'a' : 'l' : 'i' : 'g' : 'n' : 'o' : 'f' : []) = tok 9 CTokAlignof
idkwtok ('a' : 'l' : 'i' : 'g' : 'n' : 'o' : 'f' : []) = tok 7 CTokAlignof
idkwtok ('_' : '_' : 'a' : 'l' : 'i' : 'g' : 'n' : 'o' : 'f' : '_' : '_' : []) = tok 11 CTokAlignof
idkwtok ('_' : '_' : 'a' : 's' : 'm' : []) = tok 5 CTokAsm
idkwtok ('a' : 's' : 'm' : []) = tok 3 CTokAsm
idkwtok ('_' : '_' : 'a' : 's' : 'm' : '_' : '_' : []) = tok 7 CTokAsm
idkwtok ('_' : '_' : 'a' : 't' : 't' : 'r' : 'i' : 'b' : 'u' : 't' : 'e' : []) = tok 11 (CTokGnuC GnuCAttrTok)
idkwtok ('_' : '_' : 'a' : 't' : 't' : 'r' : 'i' : 'b' : 'u' : 't' : 'e' : '_' : '_' : []) = tok 13 (CTokGnuC GnuCAttrTok)
idkwtok ('a' : 'u' : 't' : 'o' : []) = tok 4 CTokAuto
idkwtok ('b' : 'r' : 'e' : 'a' : 'k' : []) = tok 5 CTokBreak
idkwtok ('_' : '_' : 'b' : 'u' : 'i' : 'l' : 't' : 'i' : 'n' : '_' : 'o' : 'f' : 'f' : 's' : 'e' : 't' : 'o' : 'f' : []) = tok 18 (CTokGnuC GnuCOffsetof)
idkwtok ('_' : '_' : 'b' : 'u' : 'i' : 'l' : 't' : 'i' : 'n' : '_' : 't' : 'y' : 'p' : 'e' : 's' : '_' : 'c' : 'o' : 'm' : 'p' : 'a' : 't' : 'i' : 'b' : 'l' : 'e' : '_' : 'p' : []) = tok 28 (CTokGnuC GnuCTyCompat)
idkwtok ('_' : '_' : 'b' : 'u' : 'i' : 'l' : 't' : 'i' : 'n' : '_' : 'v' : 'a' : '_' : 'a' : 'r' : 'g' : []) = tok 16 (CTokGnuC GnuCVaArg)
idkwtok ('c' : 'a' : 's' : 'e' : []) = tok 4 CTokCase
idkwtok ('c' : 'h' : 'a' : 'r' : []) = tok 4 CTokChar
idkwtok ('_' : '_' : 'c' : 'o' : 'm' : 'p' : 'l' : 'e' : 'x' : '_' : '_' : []) = tok 11 CTokComplex
idkwtok ('_' : '_' : 'c' : 'o' : 'n' : 's' : 't' : []) = tok 7 CTokConst
idkwtok ('c' : 'o' : 'n' : 's' : 't' : []) = tok 5 CTokConst
idkwtok ('_' : '_' : 'c' : 'o' : 'n' : 's' : 't' : '_' : '_' : []) = tok 9 CTokConst
idkwtok ('c' : 'o' : 'n' : 't' : 'i' : 'n' : 'u' : 'e' : []) = tok 8 CTokContinue
idkwtok ('d' : 'e' : 'f' : 'a' : 'u' : 'l' : 't' : []) = tok 7 CTokDefault
idkwtok ('d' : 'o' : []) = tok 2 CTokDo
idkwtok ('d' : 'o' : 'u' : 'b' : 'l' : 'e' : []) = tok 6 CTokDouble
idkwtok ('e' : 'l' : 's' : 'e' : []) = tok 4 CTokElse
idkwtok ('e' : 'n' : 'u' : 'm' : []) = tok 4 CTokEnum
idkwtok ('_' : '_' : 'e' : 'x' : 't' : 'e' : 'n' : 's' : 'i' : 'o' : 'n' : '_' : '_' : []) = tok 13 (CTokGnuC GnuCExtTok)
idkwtok ('e' : 'x' : 't' : 'e' : 'r' : 'n' : []) = tok 6 CTokExtern
idkwtok ('f' : 'l' : 'o' : 'a' : 't' : []) = tok 5 CTokFloat
idkwtok ('_' : '_' : 'f' : 'l' : 'o' : 'a' : 't' : '1' : '2' : '8' : []) = tok 10 CTokFloat128
idkwtok ('_' : 'F' : 'l' : 'o' : 'a' : 't' : '1' : '2' : '8' : []) = tok 9 CTokFloat128
idkwtok ('f' : 'o' : 'r' : []) = tok 3 CTokFor
idkwtok ('g' : 'o' : 't' : 'o' : []) = tok 4 CTokGoto
idkwtok ('i' : 'f' : []) = tok 2 CTokIf
idkwtok ('_' : '_' : 'i' : 'm' : 'a' : 'g' : []) = tok 6 (CTokGnuC GnuCComplexImag)
idkwtok ('_' : '_' : 'i' : 'm' : 'a' : 'g' : '_' : '_' : []) = tok 8 (CTokGnuC GnuCComplexImag)
idkwtok ('_' : '_' : 'i' : 'n' : 'l' : 'i' : 'n' : 'e' : []) = tok 8 CTokInline
idkwtok ('i' : 'n' : 'l' : 'i' : 'n' : 'e' : []) = tok 6 CTokInline
idkwtok ('_' : '_' : 'i' : 'n' : 'l' : 'i' : 'n' : 'e' : '_' : '_' : []) = tok 10 CTokInline
idkwtok ('i' : 'n' : 't' : []) = tok 3 CTokInt
idkwtok ('_' : '_' : 'i' : 'n' : 't' : '1' : '2' : '8' : []) = tok 8 CTokInt128
idkwtok ('_' : '_' : 'l' : 'a' : 'b' : 'e' : 'l' : '_' : '_' : []) = tok 9 CTokLabel
idkwtok ('l' : 'o' : 'n' : 'g' : []) = tok 4 CTokLong
idkwtok ('_' : '_' : 'r' : 'e' : 'a' : 'l' : []) = tok 6 (CTokGnuC GnuCComplexReal)
idkwtok ('_' : '_' : 'r' : 'e' : 'a' : 'l' : '_' : '_' : []) = tok 8 (CTokGnuC GnuCComplexReal)
idkwtok ('r' : 'e' : 'g' : 'i' : 's' : 't' : 'e' : 'r' : []) = tok 8 CTokRegister
idkwtok ('_' : '_' : 'r' : 'e' : 's' : 't' : 'r' : 'i' : 'c' : 't' : []) = tok 10 CTokRestrict
idkwtok ('r' : 'e' : 's' : 't' : 'r' : 'i' : 'c' : 't' : []) = tok 8 CTokRestrict
idkwtok ('_' : '_' : 'r' : 'e' : 's' : 't' : 'r' : 'i' : 'c' : 't' : '_' : '_' : []) = tok 12 CTokRestrict
idkwtok ('r' : 'e' : 't' : 'u' : 'r' : 'n' : []) = tok 6 CTokReturn
idkwtok ('s' : 'h' : 'o' : 'r' : 't' : []) = tok 5 CTokShort
idkwtok ('_' : '_' : 's' : 'i' : 'g' : 'n' : 'e' : 'd' : []) = tok 8 CTokSigned
idkwtok ('s' : 'i' : 'g' : 'n' : 'e' : 'd' : []) = tok 6 CTokSigned
idkwtok ('_' : '_' : 's' : 'i' : 'g' : 'n' : 'e' : 'd' : '_' : '_' : []) = tok 10 CTokSigned
idkwtok ('s' : 'i' : 'z' : 'e' : 'o' : 'f' : []) = tok 6 CTokSizeof
idkwtok ('s' : 't' : 'a' : 't' : 'i' : 'c' : []) = tok 6 CTokStatic
idkwtok ('s' : 't' : 'r' : 'u' : 'c' : 't' : []) = tok 6 CTokStruct
idkwtok ('s' : 'w' : 'i' : 't' : 'c' : 'h' : []) = tok 6 CTokSwitch
idkwtok ('_' : '_' : 't' : 'h' : 'r' : 'e' : 'a' : 'd' : []) = tok 8 CTokThread
idkwtok ('t' : 'y' : 'p' : 'e' : 'd' : 'e' : 'f' : []) = tok 7 CTokTypedef
idkwtok ('_' : '_' : 't' : 'y' : 'p' : 'e' : 'o' : 'f' : []) = tok 8 CTokTypeof
idkwtok ('t' : 'y' : 'p' : 'e' : 'o' : 'f' : []) = tok 6 CTokTypeof
idkwtok ('_' : '_' : 't' : 'y' : 'p' : 'e' : 'o' : 'f' : '_' : '_' : []) = tok 10 CTokTypeof
idkwtok ('u' : 'n' : 'i' : 'o' : 'n' : []) = tok 5 CTokUnion
idkwtok ('u' : 'n' : 's' : 'i' : 'g' : 'n' : 'e' : 'd' : []) = tok 8 CTokUnsigned
idkwtok ('v' : 'o' : 'i' : 'd' : []) = tok 4 CTokVoid
idkwtok ('_' : '_' : 'v' : 'o' : 'l' : 'a' : 't' : 'i' : 'l' : 'e' : []) = tok 10 CTokVolatile
idkwtok ('v' : 'o' : 'l' : 'a' : 't' : 'i' : 'l' : 'e' : []) = tok 8 CTokVolatile
idkwtok ('_' : '_' : 'v' : 'o' : 'l' : 'a' : 't' : 'i' : 'l' : 'e' : '_' : '_' : []) = tok 12 CTokVolatile
idkwtok ('w' : 'h' : 'i' : 'l' : 'e' : []) = tok 5 CTokWhile

idkwtok cs = \pos -> do
  name <- getNewName
  let len = case length cs of l -> l
  let ident = mkIdent pos cs name
  tyident <- isTypeIdent ident
  if tyident
    then return (CTokTyIdent (pos,len) ident)
    else return (CTokIdent   (pos,len) ident)

ignoreAttribute :: P ()
ignoreAttribute = skipTokens (0::Int)
  where skipTokens :: Int -> P ()
        skipTokens n = do
          tok <- lexToken' False
          case tok of
            CTokRParen _ | n == 1    -> return ()
                         | otherwise -> skipTokens (n-1)
            CTokLParen _             -> skipTokens (n+1)
            _                        -> skipTokens n

tok :: Int -> (PosLength -> CToken) -> Position -> P CToken
tok len tc pos = return (tc (pos,len))

adjustLineDirective :: Int -> String -> Position -> Position
adjustLineDirective pragmaLen str pos =
    offs' `seq` fname' `seq` row' `seq` (position offs' fname' row' 1)
    where
    offs'           = (posOffset pos) + pragmaLen
    str'            = dropWhite . drop 1 $ str
    (rowStr, str'') = span isDigit str'
    row'      = read rowStr
    str'''      = dropWhite str''
    fnameStr      = takeWhile (/= '"') . drop 1 $ str'''
    fname = posFile pos
    fname'      | null str''' || head str''' /= '"' = fname
     -- try and get more sharing of file name strings
     | fnameStr == fname     = fname
     | otherwise             = fnameStr
    --
    dropWhite = dropWhile (\c -> c == ' ' || c == '\t')

-- special utility for the lexer
unescapeMultiChars :: String -> [Char]
unescapeMultiChars cs@(_ : _ : _) = case unescapeChar cs of (c,cs') -> c : unescapeMultiChars cs'
unescapeMultiChars ('\'' : []) = []
unescapeMultiChars _ = error "Unexpected end of multi-char constant"

{-# INLINE token_ #-}
-- token that ignores the string
token_ :: Int -> (PosLength -> CToken) -> Position -> Int -> InputStream -> P CToken
token_ len tok pos _ _ = return (tok (pos,len))

{-# INLINE token_fail #-}
-- error token
token_fail :: String -> Position ->
              Int -> InputStream -> P CToken
token_fail errmsg pos _ _ =   failP pos [ "Lexical Error !", errmsg ]


{-# INLINE token #-}
-- token that uses the string
token :: (PosLength -> a -> CToken) -> (String -> a)
      -> Position -> Int -> InputStream -> P CToken
token tok read pos len str = return (tok (pos,len) (read $ takeChars len str))

{-# INLINE token_plus #-}
-- token that may fail
token_plus :: (PosLength -> a -> CToken) -> (String -> Either String a)
      -> Position -> Int -> InputStream -> P CToken
token_plus tok read pos len str =
  case read (takeChars len str) of Left err -> failP pos [ "Lexical error ! ", err ]
                                   Right ok -> return $! tok (pos,len) ok

-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = (Position,   -- current position,
                  InputStream)     -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

-- for alex-3.0
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (p,is) | inputStreamEmpty is = Nothing
                   | otherwise  = let (b,s) = takeByte is in
                                  -- this is safe for latin-1, but ugly
                                  let p' = alexMove p (chr (fromIntegral b)) in p' `seq`
                                  Just (b, (p', s))

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,is) | inputStreamEmpty is = Nothing
                   | otherwise  = let (c,s) = takeChar is in
                                  let p' = alexMove p c in p' `seq`
                                  Just (c, (p', s))

alexMove :: Position -> Char -> Position
alexMove pos ' '  = incPos pos 1
alexMove pos '\n' = retPos pos
alexMove pos '\r' = incOffset pos 1
alexMove pos _    = incPos pos 1

lexicalError :: P a
lexicalError = do
  pos <- getPos
  (c,cs) <- liftM takeChar getInput
  failP pos
        ["Lexical error !",
         "The character " ++ show c ++ " does not fit here."]

parseError :: P a
parseError = do
  tok <- getLastToken
  failP (posOf tok)
        ["Syntax error !",
         "The symbol `" ++ show tok ++ "' does not fit here."]

-- there is a problem with ignored tokens here (that aren't skipped)
-- consider
-- 1 > int x;
-- 2 > LINE "ex.c" 4
-- 4 > int y;
-- when we get to LINE, we have [int (1,1),x (1,4)] in the token cache.
-- Now we run
-- > action  (pos 2,0) 14 "LINE \"ex.c\" 3\n"
-- which in turn adjusts the position and then calls lexToken again
-- we get `int (pos 4,0)', and have [x (1,4), int (4,1) ] in the token cache (fine)
-- but then, we again call setLastToken when returning and get [int (4,1),int (4,1)] in the token cache (bad)
-- to resolve this, recursive calls invoke lexToken' False.
lexToken :: P CToken
lexToken = lexToken' True

lexToken' :: Bool -> P CToken
lexToken' modifyCache = do
  pos <- getPos
  inp <- getInput
  case alexScan (pos, inp) 0 of
    AlexEOF -> do
        handleEofToken
        return CTokEof
    AlexError inp' -> lexicalError
    AlexSkip  (pos', inp') len -> do
        setPos pos'
        setInput inp'
        lexToken' modifyCache
    AlexToken (pos', inp') len action -> do
        setPos pos'
        setInput inp'
        tok <- action pos len inp
        when modifyCache $ setLastToken tok
        return tok

lexC :: (CToken -> P a) -> P a
lexC cont = do
  tok <- lexToken
  cont tok
}
