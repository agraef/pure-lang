module Language.C.Analysis.TypeUtils (
    -- * Constructors
    integral,
    floating,
    simplePtr,
    size_tType,
    ptrDiffType,
    boolType,
    voidType,
    voidPtr,
    constVoidPtr,
    charPtr,
    constCharPtr,
    stringType,
    valistType,
    -- * Classifiers
    isIntegralType,
    isFloatingType,
    isPointerType,
    isScalarType,
    isFunctionType,
    -- Extractors
    typeQuals,
    typeQualsUpd,
    typeAttrs,
    typeAttrsUpd,
    baseType,
    derefTypeDef,
    deepDerefTypeDef,
    canonicalType,
    -- * Other utilities
    getIntType,
    getFloatType
) where

import Language.C.Analysis.SemRep
import Language.C.Syntax.Constants

instance Eq TypeQuals where
 (==) (TypeQuals c1 v1 r1) (TypeQuals c2 v2 r2) =
    c1 == c2 && v1 == v2 && r1 == r2

instance Ord TypeQuals where
  (<=) (TypeQuals c1 v1 r1) (TypeQuals c2 v2 r2) =
    c1 <= c2 && v1 <= v2 && r1 <= r2

-- | Constructor for a simple integral type.
integral :: IntType -> Type
integral ty = DirectType (TyIntegral ty) noTypeQuals noAttributes

-- | Constructor for a simple floating-point type.
floating :: FloatType -> Type
floating ty = DirectType (TyFloating ty) noTypeQuals noAttributes

-- | A simple pointer with no qualifiers
simplePtr :: Type -> Type
simplePtr t = PtrType t noTypeQuals []

-- | A pointer with the @const@ qualifier.
constPtr :: Type -> Type
constPtr t = PtrType t (TypeQuals True False False) []

-- | The type returned by sizeof (size_t). For now, this is just @int@.
size_tType :: Type
size_tType = integral TyInt

-- | The type of pointer differences (ptrdiff_t). For now, this is just @int@.
ptrDiffType :: Type
ptrDiffType = integral TyInt

-- | The type of comparisons\/guards. This is always just @int@.
boolType :: Type
boolType = integral TyInt

-- | Simple @void@ type.
voidType :: Type
voidType = DirectType TyVoid noTypeQuals noAttributes

-- | An unqualified @void@ pointer.
voidPtr :: Type
voidPtr = simplePtr voidType

-- | A @const@-qualified @void@ pointer.
constVoidPtr :: Type
constVoidPtr = constPtr voidType

-- | An unqualified @char@ pointer.
charPtr :: Type
charPtr = simplePtr (integral TyChar)

-- | A @const@-qualified @char@ pointer.
constCharPtr :: Type
constCharPtr = constPtr (integral TyChar)

-- | The type of a constant string.
stringType :: Type
stringType  = ArrayType
              (DirectType (TyIntegral TyChar) (TypeQuals True False False) noAttributes)
              (UnknownArraySize False)
              noTypeQuals
              []

-- | The builtin type of variable-length argument lists.
valistType :: Type
valistType  = DirectType (TyBuiltin TyVaList) noTypeQuals noAttributes

-- | Check whether a type is an integral type. This includes @enum@
--   types. This function does not attempt to resolve @typedef@ types.
isIntegralType :: Type -> Bool
isIntegralType (DirectType (TyIntegral _) _ _) = True
isIntegralType (DirectType (TyEnum _) _ _)     = True
isIntegralType _                             = False

-- | Check whether a type is a floating-point numeric type. This
--   function does not attempt to resolve @typedef@ types.
isFloatingType :: Type -> Bool
isFloatingType (DirectType (TyFloating _) _ _) = True
isFloatingType _                             = False

-- | Check whether a type is an pointer type. This includes array
--   types. This function does not attempt to resolve @typedef@ types.
isPointerType :: Type -> Bool
isPointerType (PtrType _ _ _) = True
isPointerType (ArrayType _ _ _ _) = True
isPointerType _ = False

-- | Check whether a type is a scalar type. Scalar types include
--   arithmetic types and pointer types.
isScalarType :: Type -> Bool
isScalarType t = isIntegralType t || isPointerType t || isFloatingType t

-- | return @True@ if the given type is a function type
--
--   Result is undefined in the presence of undefined typeDefs
isFunctionType :: Type -> Bool
isFunctionType ty =
    case ty of  TypeDefType (TypeDefRef _ (Just actual_ty) _) _ _ -> isFunctionType actual_ty
                TypeDefType _ _ _ -> error "isFunctionType: unresolved typeDef"
                FunctionType _ _ -> True
                _ -> False

-- | Return the qualifiers of a type.
typeQuals :: Type -> TypeQuals
typeQuals (DirectType _ q _) = q
typeQuals (PtrType _ q _) = q
typeQuals (ArrayType _ _ q _) = q
typeQuals (FunctionType _ _) = noTypeQuals
typeQuals (TypeDefType (TypeDefRef _ Nothing _)  q _) = q
typeQuals (TypeDefType (TypeDefRef _ (Just t) _) q _) = mergeTypeQuals q (typeQuals t)

--  |Update type qualifiers
--   For function types, it is an error to change any type qualifiers
--   For typedef types, the result is stored in the typedef attribute field
typeQualsUpd :: (TypeQuals -> TypeQuals) -> Type -> Type
typeQualsUpd f ty =
    case ty of DirectType ty_name ty_quals ty_attrs -> DirectType ty_name (f ty_quals) ty_attrs
               PtrType ty_inner ty_quals ty_attrs         -> PtrType ty_inner (f ty_quals) ty_attrs
               ArrayType ty_inner sz ty_quals ty_attrs    -> ArrayType ty_inner sz (f ty_quals) ty_attrs
               FunctionType ty_inner ty_attrs             -> FunctionType ty_inner ty_attrs
               TypeDefType ty_ref ty_quals ty_attrs -> TypeDefType ty_ref (f ty_quals) ty_attrs

-- | Return the attributes of a type.
typeAttrs :: Type -> Attributes
typeAttrs (DirectType _ _ a) = a
typeAttrs (PtrType _ _ a) = a
typeAttrs (ArrayType _ _ _ a) = a
typeAttrs (FunctionType _ a) = a
typeAttrs (TypeDefType (TypeDefRef _ Nothing _) _ a) = a
typeAttrs (TypeDefType (TypeDefRef _ (Just t) _) _ a) = mergeAttributes a (typeAttrs t)

--  |Update type attributes
typeAttrsUpd :: (Attributes -> Attributes) -> Type -> Type
typeAttrsUpd f ty =
    case ty of DirectType ty_name ty_quals ty_attrs -> DirectType ty_name ty_quals (f ty_attrs)
               PtrType ty_inner ty_quals ty_attrs         -> PtrType ty_inner ty_quals (f ty_attrs)
               ArrayType ty_inner sz ty_quals ty_attrs    -> ArrayType ty_inner sz ty_quals (f ty_attrs)
               FunctionType ty_inner ty_attrs             -> FunctionType ty_inner (f ty_attrs)
               TypeDefType ty_ref ty_quals ty_attrs -> TypeDefType ty_ref ty_quals (f ty_attrs)

-- | Return the base type of a pointer or array type. It is an error
--   to call this function with a type that is not in one of those two
--   categories.
baseType :: Type -> Type
baseType (PtrType t _ _) = t
baseType (ArrayType t _ _ _) = t
baseType _ = error "base of non-pointer type"

-- | resolve typedefs, if possible
derefTypeDef :: Type -> Type
derefTypeDef (TypeDefType (TypeDefRef _ (Just t) _) q a) =
  (typeAttrsUpd (mergeAttributes a) . typeQualsUpd (mergeTypeQuals q))
  (derefTypeDef t)
derefTypeDef ty = ty

-- | Attempt to remove all references to @typedef@ types from a given type.
--   Note that this does not dereference the types of structure or union
--   fields, so there are still cases where further dereferencing is
--   needed.
deepDerefTypeDef :: Type -> Type
deepDerefTypeDef (PtrType t quals attrs) =
  PtrType (deepDerefTypeDef t) quals attrs
deepDerefTypeDef (ArrayType t size quals attrs) =
  ArrayType (deepDerefTypeDef t) size quals attrs
deepDerefTypeDef (FunctionType (FunType rt params varargs) attrs) =
  FunctionType (FunType (deepDerefTypeDef rt) params varargs) attrs
deepDerefTypeDef (FunctionType (FunTypeIncomplete rt) attrs) =
  FunctionType (FunTypeIncomplete (deepDerefTypeDef rt)) attrs
deepDerefTypeDef (TypeDefType (TypeDefRef _ (Just t) _) q a) =
  (typeAttrsUpd (mergeAttributes a) . typeQualsUpd (mergeTypeQuals q))
  (deepDerefTypeDef t)
deepDerefTypeDef t = t

canonicalType :: Type -> Type
canonicalType t =
  case deepDerefTypeDef t of
    FunctionType ft attrs -> simplePtr (FunctionType ft attrs)
    t' -> t'

-- XXX: move to be with other flag functions
testFlags :: Enum f => [f] -> Flags f -> Bool
testFlags flags fi = and $ map ((flip testFlag) fi) flags

-- XXX: deal with FlagImag. No representation for it in Complex.
-- XXX: deal with invalid combinations of flags?
getIntType :: Flags CIntFlag -> IntType
getIntType flags | testFlags [FlagLongLong, FlagUnsigned] flags = TyULLong
                 | testFlag  FlagLongLong flags                 = TyLLong
                 | testFlags [FlagLong, FlagUnsigned] flags     = TyULong
                 | testFlag  FlagLong flags                     = TyLong
                 | testFlag  FlagUnsigned flags                 = TyUInt
                 | otherwise                                    = TyInt

getFloatType :: String -> FloatType
getFloatType fs | last fs `elem` ['f', 'F'] = TyFloat
                | last fs `elem` ['l', 'L'] = TyLDouble
                | otherwise                 = TyDouble

