module Language.C.Analysis.AstAnalysis where

import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Syntax.AST

data StmtCtx = FunCtx VarDecl
             | LoopCtx
             | SwitchCtx

data ExprSide = LValue | RValue

tExpr :: MonadTrav m => [StmtCtx] -> ExprSide -> CExpr -> m Type
