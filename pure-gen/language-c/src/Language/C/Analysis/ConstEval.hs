{-# LANGUAGE RelaxedPolyRec #-}
module Language.C.Analysis.ConstEval where

import Control.Monad
import Data.Bits
import Data.Maybe
import qualified Data.Map as Map
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import {-# SOURCE #-} Language.C.Analysis.AstAnalysis (tExpr, ExprSide(..))
import Language.C.Analysis.Debug
import Language.C.Analysis.DeclAnalysis
import Language.C.Analysis.DefTable
import Language.C.Data
import Language.C.Pretty
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Analysis.TypeUtils
import Text.PrettyPrint.HughesPJ

data MachineDesc =
  MachineDesc
  { iSize        :: IntType -> Integer
  , fSize        :: FloatType -> Integer
  , builtinSize  :: BuiltinType -> Integer
  , ptrSize      :: Integer
  , voidSize     :: Integer
  , iAlign       :: IntType -> Integer
  , fAlign       :: FloatType -> Integer
  , builtinAlign :: BuiltinType -> Integer
  , ptrAlign     :: Integer
  , voidAlign    :: Integer
  }

intExpr :: (Pos n, MonadName m) => n -> Integer -> m CExpr
intExpr n i =
  genName >>= \name ->
    return $ CConst $ CIntConst (cInteger i) (mkNodeInfo (posOf n) name)

sizeofType :: (MonadTrav m, CNode n) => MachineDesc -> n -> Type -> m Integer
sizeofType md _ (DirectType TyVoid _ _) = return $ voidSize md
sizeofType md _ (DirectType (TyIntegral it) _ _) = return $ iSize md it
sizeofType md _ (DirectType (TyFloating ft) _ _) = return $ fSize md ft
sizeofType md _ (DirectType (TyComplex ft) _ _) = return $ 2 * fSize md ft
sizeofType md _ (DirectType (TyComp ctr) _ _) = compSize md ctr
sizeofType md _ (DirectType (TyEnum _) _ _) = return $ iSize md TyInt
sizeofType md _ (DirectType (TyBuiltin b) _ _) = return $ builtinSize md b
sizeofType md _ (PtrType _ _ _)  = return $ ptrSize md
sizeofType md n (ArrayType bt (UnknownArraySize _) _ _) = return $ ptrSize md
sizeofType md n (ArrayType bt (ArraySize _ sz) _ _) =
  do sz' <- constEval md Map.empty sz
     case sz' of
       CConst (CIntConst i _) ->
         do s <- sizeofType md n bt
            return $ getCInteger i * s
       _ -> return $ ptrSize md
            {-
            astError (nodeInfo sz) $
            "array size is not a constant: " ++ (render . pretty) sz
            -}
sizeofType md n (TypeDefType (TypeDefRef _ (Just t) _) _ _) = sizeofType md n t
sizeofType md _ (FunctionType _ _) = return $ ptrSize md
sizeofType _ n t = astError (nodeInfo n) $
                 "can't find size of type: " ++ (render . pretty) t

alignofType :: (MonadTrav m, CNode n) => MachineDesc -> n -> Type -> m Integer
alignofType md _ (DirectType TyVoid _ _) = return $ voidAlign md
alignofType md _ (DirectType (TyIntegral it) _ _) = return $ iAlign md it
alignofType md _ (DirectType (TyFloating ft) _ _) = return $ fAlign md ft
alignofType md _ (DirectType (TyComplex ft) _ _) = return $ fAlign md ft
alignofType md _ (DirectType (TyEnum _) _ _) = return $ iAlign md TyInt
alignofType md _ (DirectType (TyBuiltin b) _ _) = return $ builtinAlign md b
alignofType md _ (PtrType _ _ _)  = return $ ptrAlign md
alignofType md n (ArrayType bt (UnknownArraySize _) _ _) = return $ ptrAlign md
alignofType md n (ArrayType bt (ArraySize _ sz) _ _) = alignofType md n bt
alignofType md n (TypeDefType (TypeDefRef _ (Just t) _) _ _) = alignofType md n t
alignofType _ n t = astError (nodeInfo n) $
                 "can't find alignment of type: " ++ (render . pretty) t

compSize :: MonadTrav m => MachineDesc -> CompTypeRef -> m Integer
compSize md ctr =
  do dt <- getDefTable
     case lookupTag (sueRef ctr) dt of
       Just (Left _)   -> astError (nodeInfo ctr)
                          "composite declared but not defined"
       Just (Right (CompDef (CompType _ tag ms _ ni))) ->
         do let ts = map declType ms
            sizes <- mapM (sizeofType md ni) ts
            -- XXX: handle padding
            case tag of
              StructTag -> return $ sum sizes
              UnionTag  -> return $ maximum sizes
       Just (Right (EnumDef _)) -> return $ iSize md TyInt
       Nothing         -> astError (nodeInfo ctr) "unknown composite"


{- Expression evaluation -}

-- Use the withWordBytes function to wrap the results around to the
-- correct word size
intOp :: CBinaryOp -> Integer -> Integer -> Integer
intOp CAddOp i1 i2 = i1 + i2
intOp CSubOp i1 i2 = i1 - i2
intOp CMulOp i1 i2 = i1 * i2
intOp CDivOp i1 i2 = i1 `div` i2
intOp CRmdOp i1 i2 = i1 `mod` i2
intOp CShlOp i1 i2 = i1 `shiftL` fromInteger i2
intOp CShrOp i1 i2 = i1 `shiftR` fromInteger i2
intOp CLeOp  i1 i2 = toInteger $ fromEnum $ i1 < i2
intOp CGrOp  i1 i2 = toInteger $ fromEnum $ i1 > i2
intOp CLeqOp i1 i2 = toInteger $ fromEnum $ i1 <= i2
intOp CGeqOp i1 i2 = toInteger $ fromEnum $ i1 >= i2
intOp CEqOp  i1 i2 = toInteger $ fromEnum $ i1 == i2
intOp CNeqOp i1 i2 = toInteger $ fromEnum $ i1 /= i2
intOp CAndOp i1 i2 = i1 .&. i2
intOp CXorOp i1 i2 = i1 `xor` i2
intOp COrOp  i1 i2 = i1 .|. i2
intOp CLndOp i1 i2 = toInteger $ fromEnum $ (i1 /= 0) && (i2 /= 0)
intOp CLorOp i1 i2 = toInteger $ fromEnum $ (i1 /= 0) || (i2 /= 0)

-- Use the withWordBytes function to wrap the results around to the
-- correct word size
intUnOp :: CUnaryOp -> Integer -> Maybe Integer
intUnOp CPlusOp i = Just i
intUnOp CMinOp  i = Just $ -i
intUnOp CCompOp i = Just $ complement i
intUnOp CNegOp  i = Just $ toInteger $ fromEnum $ i == 0
intUnOp _       _ = Nothing

withWordBytes :: Int -> Integer -> Integer
withWordBytes bytes n = n `rem` (1 `shiftL` (bytes `shiftL` 3))

boolValue :: CExpr -> Maybe Bool
boolValue (CConst (CIntConst i _))  = Just $ getCInteger i /= 0
boolValue (CConst (CCharConst c _)) = Just $ getCCharAsInt c /= 0
boolValue (CConst (CStrConst _ _))  = Just True
boolValue _                         = Nothing

intValue :: CExpr -> Maybe Integer
intValue (CConst (CIntConst i _))  = Just $ getCInteger i
intValue (CConst (CCharConst c _)) = Just $ getCCharAsInt c
intValue _                         = Nothing

constEval :: (MonadTrav m) =>
             MachineDesc -> Map.Map Ident CExpr -> CExpr -> m CExpr
constEval md env (CCond e1 me2 e3 ni) =
  do e1'  <- constEval md env e1
     me2' <- maybe (return Nothing) (\e -> Just `liftM` constEval md env e) me2
     e3'  <- constEval md env e3
     case boolValue e1' of
       Just True  -> return $ fromMaybe e1' me2'
       Just False -> return e3'
       Nothing    -> return $ CCond e1' me2' e3' ni
constEval md env e@(CBinary op e1 e2 ni) =
  do e1' <- constEval md env e1
     e2' <- constEval md env e2
     t <- tExpr [] RValue e
     bytes <- fromIntegral `liftM` sizeofType md e t
     case (intValue e1', intValue e2') of
       (Just i1, Just i2) -> intExpr ni (withWordBytes bytes (intOp op i1 i2))
       (_, _)             -> return $ CBinary op e1' e2' ni
constEval md env (CUnary op e ni) =
  do e' <- constEval md env e
     t <- tExpr [] RValue e
     bytes <- fromIntegral `liftM` sizeofType md e t
     case intValue e' of
       Just i  -> case intUnOp op i of
                    Just i' -> intExpr ni (withWordBytes bytes i')
                    Nothing -> astError ni
                               "invalid unary operator applied to constant"
       Nothing -> return $ CUnary op e' ni
constEval md env (CCast d e ni) =
  do e' <- constEval md env e
     t <- analyseTypeDecl d
     bytes <- fromIntegral `liftM` sizeofType md d t
     case intValue e' of
       Just i -> intExpr ni (withWordBytes bytes i)
       Nothing -> return $ CCast d e' ni
constEval md _ (CSizeofExpr e ni) =
  do t <- tExpr [] RValue e
     sz <- sizeofType md e t
     intExpr ni sz
constEval md _ (CSizeofType d ni) =
  do t <- analyseTypeDecl d
     sz <- sizeofType md d t
     intExpr ni sz
constEval md _ (CAlignofExpr e ni) =
  do t <- tExpr [] RValue e
     sz <- alignofType md e t
     intExpr ni sz
constEval md _ (CAlignofType d ni) =
  do t <- analyseTypeDecl d
     sz <- alignofType md d t
     intExpr ni sz
constEval md env e@(CVar i _) | Map.member i env =
  return $ fromMaybe e $ Map.lookup i env
constEval md env e@(CVar i _) =
  do t <- tExpr [] RValue e
     case derefTypeDef t of
       DirectType (TyEnum etr) _ _ ->
         do dt <- getDefTable
            case lookupTag (sueRef etr) dt of
              Just (Right (EnumDef (EnumType _ es _ _))) ->
                do env' <- foldM enumConst env es
                   return $ fromMaybe e $ Map.lookup i env'
              _ -> return e
       _ -> return e
  where enumConst env' (Enumerator n e' _ _) =
          do c <- constEval md env' e'
             return $ Map.insert n c env'
constEval _ _ e = return e
