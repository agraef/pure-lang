-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax.ParserMonad
-- Copyright   :  (c) [1999..2004] Manuel M T Chakravarty
--                (c) 2005-2007 Duncan Coutts
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- Monad for the C lexer and parser
--
--  This monad has to be usable with Alex and Happy. Some things in it are
--  dictated by that, eg having to be able to remember the last token.
--
--  The monad also provides a unique name supply (via the Name module)
--
--  For parsing C we have to maintain a set of identifiers that we know to be
--  typedef'ed type identifiers. We also must deal correctly with scope so we
--  keep a list of sets of identifiers so we can save the outer scope when we
--  enter an inner scope.
module Language.C.Parser.ParserMonad (
  P,
  execParser,
  failP,
  getNewName,        -- :: P Name
  addTypedef,        -- :: Ident -> P ()
  shadowTypedef,     -- :: Ident -> P ()
  isTypeIdent,       -- :: Ident -> P Bool
  enterScope,        -- :: P ()
  leaveScope,        -- :: P ()
  setPos,            -- :: Position -> P ()
  getPos,            -- :: P Position
  getInput,          -- :: P String
  setInput,          -- :: String -> P ()
  getLastToken,      -- :: P CToken
  getSavedToken,     -- :: P CToken
  setLastToken,      -- :: CToken -> P ()
  handleEofToken,    -- :: P ()
  getCurrentPosition,-- :: P Position
  ParseError(..),
  ) where
import Language.C.Data.Error (internalErr, showErrorInfo,ErrorInfo(..),ErrorLevel(..))
import Language.C.Data.Position  (Position(..))
import Language.C.Data.InputStream
import Language.C.Data.Name    (Name)
import Language.C.Data.Ident    (Ident)
import Language.C.Parser.Tokens (CToken(CTokEof))

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Data.Set  (Set)
import qualified Data.Set as Set (fromList, insert, member, delete)

newtype ParseError = ParseError ([String],Position)
instance Show ParseError where
    show (ParseError (msgs,pos)) = showErrorInfo "Syntax Error !" (ErrorInfo LevelError pos msgs)


data ParseResult a
  = POk !PState a
  | PFailed [String] Position   -- The error message and position

data PState = PState {
        curPos     :: !Position,        -- position at current input location
        curInput   :: !InputStream,      -- the current input
        prevToken  ::  CToken,          -- the previous token
        savedToken ::  CToken,          -- and the token before that
        namesupply :: ![Name],          -- the name unique supply
        tyidents   :: !(Set Ident),     -- the set of typedef'ed identifiers
        scopes     :: ![Set Ident]      -- the tyident sets for outer scopes
     }

newtype P a = P { unP :: PState -> ParseResult a }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = return
  (<*>) = ap

instance Monad P where
  return = returnP
  (>>=) = thenP
  fail m = getPos >>= \pos -> failP pos [m]


-- | execute the given parser on the supplied input stream.
--   returns 'ParseError' if the parser failed, and a pair of
--   result and remaining name supply otherwise
--
-- Synopsis: @execParser parser inputStream initialPos predefinedTypedefs uniqNameSupply@
execParser :: P a -> InputStream -> Position -> [Ident] -> [Name]
           -> Either ParseError (a,[Name])
execParser (P parser) input pos builtins names =
  case parser initialState of
    PFailed message errpos -> Left (ParseError (message,errpos))
    POk st result -> Right (result, namesupply st)
  where initialState = PState {
          curPos = pos,
          curInput = input,
          prevToken = internalErr "CLexer.execParser: Touched undefined token!",
          savedToken = internalErr "CLexer.execParser: Touched undefined token (safed token)!",
          namesupply = names,
          tyidents = Set.fromList builtins,
          scopes   = []
        }

{-# INLINE returnP #-}
returnP :: a -> P a
returnP a = P $ \s -> POk s a

{-# INLINE thenP #-}
thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \s ->
        case m s of
                POk s' a        -> (unP (k a)) s'
                PFailed err pos -> PFailed err pos

failP :: Position -> [String] -> P a
failP pos msg = P $ \_ -> PFailed msg pos

getNewName :: P Name
getNewName = P $ \s@PState{namesupply=(n:ns)} -> n `seq` POk s{namesupply=ns} n

setPos :: Position -> P ()
setPos pos = P $ \s -> POk s{curPos=pos} ()

getPos :: P Position
getPos = P $ \s@PState{curPos=pos} -> POk s pos

addTypedef :: Ident -> P ()
addTypedef ident = (P $ \s@PState{tyidents=tyids} ->
                             POk s{tyidents = ident `Set.insert` tyids} ())

shadowTypedef :: Ident -> P ()
shadowTypedef ident = (P $ \s@PState{tyidents=tyids} ->
                             -- optimisation: mostly the ident will not be in
                             -- the tyident set so do a member lookup to avoid
                             --  churn induced by calling delete
                             POk s{tyidents = if ident `Set.member` tyids
                                                then ident `Set.delete` tyids
                                                else tyids } ())

isTypeIdent :: Ident -> P Bool
isTypeIdent ident = P $ \s@PState{tyidents=tyids} ->
                             POk s $! Set.member ident tyids

enterScope :: P ()
enterScope = P $ \s@PState{tyidents=tyids,scopes=ss} ->
                     POk s{scopes=tyids:ss} ()

leaveScope :: P ()
leaveScope = P $ \s@PState{scopes=ss} ->
                     case ss of
                       []          -> error "leaveScope: already in global scope"
                       (tyids:ss') -> POk s{tyidents=tyids, scopes=ss'} ()

getInput :: P InputStream
getInput = P $ \s@PState{curInput=i} -> POk s i

setInput :: InputStream -> P ()
setInput i = P $ \s -> POk s{curInput=i} ()

getLastToken :: P CToken
getLastToken = P $ \s@PState{prevToken=tok} -> POk s tok

getSavedToken :: P CToken
getSavedToken = P $ \s@PState{savedToken=tok} -> POk s tok

-- | @setLastToken modifyCache tok@
setLastToken :: CToken -> P ()
setLastToken CTokEof = P $ \s -> POk s{savedToken=(prevToken s)} ()
setLastToken tok      = P $ \s -> POk s{prevToken=tok,savedToken=(prevToken s)} ()

-- | handle an End-Of-File token (changes savedToken)
handleEofToken :: P ()
handleEofToken = P $ \s -> POk s{savedToken=(prevToken s)} ()

getCurrentPosition :: P Position
getCurrentPosition = P $ \s@PState{curPos=pos} -> POk s pos