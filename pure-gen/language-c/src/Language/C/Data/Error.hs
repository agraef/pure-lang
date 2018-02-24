{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Data.Error
-- Copyright   :  (c) 2008 Benedikt Huber, Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- Base type for errors occurring in parsing, analysing and pretty-printing.
-- With ideas from Simon Marlow's
-- "An extensible dynamically-typed hierarchy of execeptions [2006]"
-----------------------------------------------------------------------------
module Language.C.Data.Error (
    -- * Severity Level
    ErrorLevel(..), isHardError,
    -- * Error class
    Error(..), errorPos, errorLevel, errorMsgs,
    -- * Error 'supertype'
    CError(..),
    -- * Infos attached to errors
    ErrorInfo(..),showError,showErrorInfo,mkErrorInfo,
    -- * Default error types
    UnsupportedFeature, unsupportedFeature, unsupportedFeature_,
    UserError, userErr,
    -- * Raising internal errors
    internalErr,
)
where
import Data.Typeable
import Data.Generics
import Language.C.Data.Node
import Language.C.Data.Position

-- | Error levels (severity)
data ErrorLevel = LevelWarn
                | LevelError
                | LevelFatal
              deriving (Eq, Ord)

instance Show ErrorLevel where
    show LevelWarn  = "WARNING"
    show LevelError = "ERROR"
    show LevelFatal = "FATAL ERROR"

-- | return @True@ when the given error makes it impossible to continue
--   analysis or compilation.
isHardError :: (Error ex) => ex -> Bool
isHardError = ( > LevelWarn) . errorLevel

-- | information attached to every error in Language.C
data ErrorInfo = ErrorInfo ErrorLevel Position [String] deriving Typeable

-- to facilitate newtype deriving
instance Show ErrorInfo where show = showErrorInfo "error"
instance Error ErrorInfo where
    errorInfo = id
    changeErrorLevel (ErrorInfo _ pos msgs) lvl' = ErrorInfo lvl' pos msgs

mkErrorInfo :: ErrorLevel -> String -> NodeInfo -> ErrorInfo
mkErrorInfo lvl msg node = ErrorInfo lvl (posOfNode node) (lines msg)

-- | `supertype' of all errors
data CError
    = forall err. (Error err) => CError err
    deriving Typeable

-- | errors in Language.C are instance of 'Error'
class (Typeable e, Show e) => Error e where
    -- | obtain source location etc. of an error
    errorInfo        :: e -> ErrorInfo
    -- | wrap error in 'CError'
    toError          :: e -> CError
    -- | try to cast a generic 'CError' to the specific error type
    fromError     :: CError -> (Maybe e)
    -- | modify the error level
    changeErrorLevel :: e -> ErrorLevel -> e

    -- default implementation
    fromError (CError e) = cast e
    toError = CError
    changeErrorLevel e lvl =
        if errorLevel e == lvl
            then e
            else error $ "changeErrorLevel: not possible for " ++ show e

instance Show CError where
    show (CError e) = show e
instance Error CError where
    errorInfo (CError err) = errorInfo err
    toError = id
    fromError = Just
    changeErrorLevel (CError e) = CError . changeErrorLevel e

-- | position of an @Error@
errorPos   :: (Error e) => e -> Position
errorPos = ( \(ErrorInfo _ pos _) -> pos ) . errorInfo

-- | severity level of an @Error@
errorLevel :: (Error e) => e -> ErrorLevel
errorLevel = ( \(ErrorInfo lvl _ _) -> lvl ) . errorInfo

-- | message lines of an @Error@
errorMsgs   :: (Error e) => e -> [String]
errorMsgs = ( \(ErrorInfo _ _ msgs) -> msgs ) . errorInfo

-- | error raised if a operation requires an unsupported or not yet implemented feature.
data UnsupportedFeature = UnsupportedFeature String Position deriving Typeable
instance Error UnsupportedFeature where
    errorInfo (UnsupportedFeature msg pos) = ErrorInfo LevelError pos (lines msg)
instance Show UnsupportedFeature where show = showError "Unsupported Feature"

unsupportedFeature :: (Pos a) => String -> a -> UnsupportedFeature
unsupportedFeature msg a = UnsupportedFeature msg (posOf a)

unsupportedFeature_ :: String -> UnsupportedFeature
unsupportedFeature_ msg = UnsupportedFeature msg internalPos

-- | unspecified error raised by the user (in case the user does not want to define
--   her own error types).
newtype UserError     = UserError ErrorInfo deriving Typeable
instance Error UserError where
    errorInfo (UserError info) = info
instance Show UserError where show = showError "User Error"

userErr :: String -> UserError
userErr msg = UserError (ErrorInfo LevelError internalPos (lines msg))

-- other errors to be defined elsewhere

showError :: (Error e) => String -> e -> String
showError short_msg = showErrorInfo short_msg . errorInfo

-- | converts an error into a string using a fixed format
--
-- * either the lines of the long error message or the short message has to be non-empty
--
-- * the format is
--
-- >    <fname>:<row>: (column <col>) [<err lvl>]
-- >      >>> <line_1>
-- >      <line_2>
-- >        ...
-- >      <line_n>
showErrorInfo :: String -> ErrorInfo -> String
showErrorInfo short_msg (ErrorInfo level pos msgs) =
    header ++ showMsgLines (if null short_msg then msgs else short_msg:msgs)
    where
    header = showPos pos ++ "[" ++ show level ++ "]"
    showPos p | isSourcePos p = (posFile p) ++ ":" ++ show (posRow pos) ++ ": " ++
                                "(column " ++ show (posColumn pos) ++ ") "
              | otherwise = show p ++ ":: "
    showMsgLines []     = internalErr "No short message or error message provided."
    showMsgLines (x:xs) = indent ++ ">>> " ++ x ++ "\n" ++ unlines (map (indent++) xs)


-- internal errors
internalErrPrefix :: String
internalErrPrefix = unlines [ "Language.C : Internal Error" ,
                              "This is propably a bug, and should be reported at "++
                              "http://www.sivity.net/projects/language.c/newticket"]

-- | raise a fatal internal error; message may have multiple lines
internalErr     :: String -> a
internalErr msg  = error (internalErrPrefix ++ "\n"
                       ++ indentLines msg
                       ++ "\n")
indent :: String
indent = "  "
indentLines :: String -> String
indentLines = unlines . map (indent++) . lines
