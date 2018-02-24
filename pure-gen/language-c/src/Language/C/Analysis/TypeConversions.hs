module Language.C.Analysis.TypeConversions (
    arithmeticConversion,
    floatConversion,
    intConversion
) where

import Language.C.Analysis.SemRep

-- | For an arithmetic operator, if the arguments are of the given
--   types, return the type of the full expression.
arithmeticConversion :: TypeName -> TypeName -> Maybe TypeName
-- XXX: I'm assuming that double `op` complex float = complex
-- double. The standard seems somewhat unclear on whether this is
-- really the case.
arithmeticConversion (TyComplex t1) (TyComplex t2) =
  Just $ TyComplex $ floatConversion t1 t2
arithmeticConversion (TyComplex t1) (TyFloating t2) =
  Just $ TyComplex $ floatConversion t1 t2
arithmeticConversion (TyFloating t1) (TyComplex t2) =
  Just $ TyComplex $ floatConversion t1 t2
arithmeticConversion t1@(TyComplex _) (TyIntegral _) = Just t1
arithmeticConversion (TyIntegral _) t2@(TyComplex _) = Just t2
arithmeticConversion (TyFloating t1) (TyFloating t2) =
  Just $ TyFloating $ floatConversion t1 t2
arithmeticConversion t1@(TyFloating _) (TyIntegral _) = Just t1
arithmeticConversion (TyIntegral _) t2@(TyFloating _) = Just t2
arithmeticConversion (TyIntegral t1) (TyIntegral t2) =
  Just $ TyIntegral $ intConversion t1 t2
arithmeticConversion (TyEnum _) (TyEnum _) = Just $ TyIntegral TyInt
arithmeticConversion (TyEnum _) t2 = Just $ t2
arithmeticConversion t1 (TyEnum _) = Just $ t1
arithmeticConversion _ _ = Nothing

floatConversion :: FloatType -> FloatType -> FloatType
floatConversion = max

intConversion :: IntType -> IntType -> IntType
intConversion t1 t2 = max TyInt (max t1 t2)

