{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.SemError
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  alpha
-- Portability :  ghc
--
-- Errors in the semantic analysis
-----------------------------------------------------------------------------
module Language.C.Analysis.SemError (
InvalidASTError(..), invalidAST,
BadSpecifierError(..), badSpecifierError,
TypeMismatch(..), typeMismatch,
RedefError(..), RedefInfo(..), RedefKind(..), redefinition,
)
where
import Data.Typeable

-- this means we cannot use SemError in SemRep, but use rich types here
import Language.C.Analysis.SemRep

import Language.C.Data.Error
import Language.C.Data.Node

-- here are the errors available

-- | InvalidASTError is caused by the violation of an invariant in the AST
newtype InvalidASTError = InvalidAST ErrorInfo deriving (Typeable)

instance Error InvalidASTError where
    errorInfo (InvalidAST ei) = ei
    changeErrorLevel (InvalidAST ei) lvl' = InvalidAST (changeErrorLevel ei lvl')

-- | BadSpecifierError is caused by an invalid combination of specifiers
newtype BadSpecifierError = BadSpecifierError ErrorInfo deriving (Typeable)

instance Error BadSpecifierError where
    errorInfo (BadSpecifierError ei) = ei
    changeErrorLevel (BadSpecifierError ei) lvl' = BadSpecifierError (changeErrorLevel ei lvl')

-- | RedefError is caused by an invalid redefinition of the same identifier or type
data RedefError = RedefError ErrorLevel RedefInfo deriving Typeable

data RedefInfo = RedefInfo String RedefKind NodeInfo NodeInfo
data RedefKind = DuplicateDef | DiffKindRedecl | ShadowedDef | DisagreeLinkage |
                 NoLinkageOld
data TypeMismatch = TypeMismatch String (NodeInfo,Type) (NodeInfo,Type) deriving Typeable

-- Invalid AST
-- ~~~~~~~~~~~

instance Show InvalidASTError  where show = showError "AST invariant violated"

invalidAST :: NodeInfo -> String -> InvalidASTError
invalidAST node_info msg = InvalidAST (mkErrorInfo LevelError msg node_info)

-- Bad specifier (e.g. static for a parameter, or extern when there is an initializer)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

instance Show BadSpecifierError     where show = showError "Bad specifier"

badSpecifierError :: NodeInfo -> String -> BadSpecifierError
badSpecifierError node_info msg = BadSpecifierError (mkErrorInfo LevelError msg node_info)

-- Type mismatch
-- ~~~~~~~~~~~~~
typeMismatch :: String -> (NodeInfo, Type) -> (NodeInfo,Type) -> TypeMismatch
typeMismatch = TypeMismatch

instance Show TypeMismatch where
    show tm = showError "Type mismatch" (typeMismatchInfo tm)
instance Error TypeMismatch where
    errorInfo = typeMismatchInfo
typeMismatchInfo :: TypeMismatch -> ErrorInfo
typeMismatchInfo (TypeMismatch reason (node1,_ty2) _t2) =
    ErrorInfo LevelError (posOfNode node1) [reason]

-- Redefinitions
-- ~~~~~~~~~~~~~

instance Show RedefError  where
    show (RedefError lvl info) = showErrorInfo (redefErrLabel info) (redefErrorInfo lvl info)
instance Error RedefError where
    errorInfo (RedefError lvl info) = redefErrorInfo lvl info
    changeErrorLevel (RedefError _lvl info) lvl' = RedefError lvl' info

redefErrLabel :: RedefInfo -> String
redefErrLabel  (RedefInfo ident _ _ _) = ident ++ " redefined"

redefErrorInfo :: ErrorLevel -> RedefInfo -> ErrorInfo
redefErrorInfo lvl info@(RedefInfo _ _ node old_node) =
    ErrorInfo lvl (posOfNode node) ([redefErrReason info] ++ prevDeclMsg old_node)

redefErrReason :: RedefInfo -> String
redefErrReason (RedefInfo ident DuplicateDef _ _) = "duplicate definition of " ++ ident
redefErrReason (RedefInfo ident ShadowedDef _ _)   = "this declaration of " ++ ident ++ " shadows a previous one"
redefErrReason (RedefInfo ident DiffKindRedecl _ _) = ident ++ " previously declared as a different kind of symbol"
redefErrReason (RedefInfo ident DisagreeLinkage _ _) = ident ++ " previously declared with different linkage"
redefErrReason (RedefInfo ident NoLinkageOld _ _) = ident ++ " previously declared without linkage"

prevDeclMsg :: NodeInfo -> [String]
prevDeclMsg old_node = ["The previous declaration was here: ", show (posOfNode old_node)]

redefinition :: ErrorLevel -> String -> RedefKind -> NodeInfo -> NodeInfo -> RedefError
redefinition lvl ctx kind new old = RedefError lvl (RedefInfo ctx kind new old)

