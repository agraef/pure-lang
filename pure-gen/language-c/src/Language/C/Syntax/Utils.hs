module Language.C.Syntax.Utils (
  -- * Generic operations
  getSubStmts,
  mapSubStmts,
  mapBlockItemStmts,
  -- * Concrete operations
  getLabels
) where

import Data.List
import Language.C.Data.Ident
import Language.C.Syntax.AST

-- XXX: This is should be generalized !!!
--      Data.Generics sounds attractive, but we really need to control the evaluation order
-- XXX: Expression statements (which are somewhat problematic anyway), aren't handled yet
getSubStmts :: CStat -> [CStat]
getSubStmts (CLabel _ s _ _)      = [s]
getSubStmts (CCase _ s _)         = [s]
getSubStmts (CCases _ _ s _)      = [s]
getSubStmts (CDefault s _)        = [s]
getSubStmts (CExpr _ _)           = []
getSubStmts (CCompound _ body _)  = concatMap compoundSubStmts body
getSubStmts (CIf _ sthen selse _) = maybe [sthen] (\s -> [sthen,s]) selse
getSubStmts (CSwitch _ s _)       = [s]
getSubStmts (CWhile _ s _ _)      = [s]
getSubStmts (CFor _ _ _ s _)      = [s]
getSubStmts (CGoto _ _)           = []
getSubStmts (CGotoPtr _ _)        = []
getSubStmts (CCont _)             = []
getSubStmts (CBreak _)            = []
getSubStmts (CReturn _ _)         = []
getSubStmts (CAsm _ _)            = []

mapSubStmts :: (CStat -> Bool) -> (CStat -> CStat) -> CStat -> CStat
mapSubStmts stop _ s | stop s = s
mapSubStmts stop f (CLabel i s attrs ni) =
  f (CLabel i (mapSubStmts stop f s) attrs ni)
mapSubStmts stop f (CCase e s ni) =
  f (CCase e (mapSubStmts stop f s) ni)
mapSubStmts stop f (CCases e1 e2 s ni) =
  f (CCases e1 e2 (mapSubStmts stop f s) ni)
mapSubStmts stop f (CDefault s ni) =
  f (CDefault (mapSubStmts stop f s) ni)
mapSubStmts stop f (CCompound ls body ni) =
  f (CCompound ls (map (mapBlockItemStmts stop f) body) ni)
mapSubStmts stop f (CIf e sthen selse ni) =
  f (CIf e
     (mapSubStmts stop f sthen)
     (maybe Nothing (Just . mapSubStmts stop f) selse)
     ni)
mapSubStmts stop f (CSwitch e s ni) =
  f (CSwitch e (mapSubStmts stop f s) ni)
mapSubStmts stop f (CWhile e s isdo ni) =
  f (CWhile e (mapSubStmts stop f s) isdo ni)
mapSubStmts stop f (CFor i t a s ni) =
  f (CFor i t a (mapSubStmts stop f s) ni)
mapSubStmts _ f s  = f s

mapBlockItemStmts :: (CStat -> Bool)
                  -> (CStat -> CStat)
                  -> CBlockItem
                  -> CBlockItem
mapBlockItemStmts stop f (CBlockStmt s) = CBlockStmt (mapSubStmts stop f s)
mapBlockItemStmts _ _ bi                = bi

compoundSubStmts :: CBlockItem -> [CStat]
compoundSubStmts (CBlockStmt s)    = [s]
compoundSubStmts (CBlockDecl _)    = []
compoundSubStmts (CNestedFunDef _) = []

getLabels :: CStat -> [Ident]
getLabels (CLabel l s _ _)      = l : getLabels s
getLabels (CCompound ls body _) =
  concatMap (concatMap getLabels . compoundSubStmts) body \\ ls
getLabels stmt                  = concatMap getLabels (getSubStmts stmt)
