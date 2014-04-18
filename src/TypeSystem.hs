module TypeSystem(
	Type, computeType, TypeConstraint, typeConstraint, dimension,
	typeVar, defMatrix, genMatrix, unify, applySubstitutions,
	leftDefMatrix, rightDefMatrix, func, uniqueTypeNames, isTypeVar,
	rowDimension, colDimension, varName, dimNum) where

import Data.Char
import Data.List
import ErrorHandling

data Type
	= Matrix Type Type
	| TypeVar String
	| Func Type Type
	| Dimension Int
	deriving (Eq)

instance Show Type where
	show = showType

rowDimension (Matrix t1 t2) = t1
colDimension (Matrix t1 t2) = t2

varName (TypeVar t) = t
dimNum (Dimension d) = d

isDefTypeVar :: Type -> Bool
isDefTypeVar (TypeVar name) = isUpper (head name)
isDefTypeVar _ = False

-- Iterate over type giving unique names to TypeVars based on input string
uniqueTypeNames :: String -> Type -> Type
uniqueTypeNames pref t = rename (assignNames [] pref t) t

rename :: [(String, String)] -> Type -> Type
rename name (Func t1 t2) = Func (rename name t1) (rename name t2)
rename name (Matrix t1 t2) = Matrix (rename name t1) (rename name t2)
rename names (TypeVar name) = case lookup name names of
	Just n -> (TypeVar n)
	Nothing -> error $ show name ++ " has no unique name"
rename _ t = t

assignNames already pref (Func t1 t2) = names
	where
		nextNames = assignNames already (pref ++ "1") t1
		names = assignNames nextNames (pref ++ "2") t2
assignNames already pref (Matrix t1 t2) = names
	where
		nextNames = assignNames already (pref ++ "1") t1
		names = assignNames nextNames (pref ++ "2") t2
assignNames already pref (TypeVar name) = case lookup name already of
	Just uniqueName -> already
	Nothing -> ((name, pref ++ "-" ++ name):already)
assignNames _ _ t = []


isMatrix :: Type -> Bool
isMatrix (Matrix _ _) = True
isMatrix _ = False

isTypeVar :: Type -> Bool
isTypeVar (TypeVar _) = True
isTypeVar _ = False

dimension :: Int -> Type
dimension n = Dimension n

defMatrix :: Int -> Int -> Type
defMatrix r c = Matrix (dimension r) (dimension c)

genMatrix :: String -> String -> Type
genMatrix v1 v2 = Matrix (typeVar v1) (typeVar v2)

leftDefMatrix :: Int -> String -> Type
leftDefMatrix r v2 = Matrix (dimension r) (typeVar v2)

rightDefMatrix :: String -> Int -> Type
rightDefMatrix v1 c = Matrix (typeVar v1) (dimension c)

typeVar name = TypeVar name

func t1 t2 = Func t1 t2

showType :: Type -> String
showType (TypeVar name) = name
showType (Matrix r c) = "M " ++ show r ++ " " ++ show c
showType (Func t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
showType (Dimension n) = show n

type TypeConstraint = (Type, Type)

typeConstraint :: Type -> Type -> TypeConstraint
typeConstraint t1 t2 = (t1, t2)

computeType :: [TypeConstraint] -> Error Type
computeType constraints = Succeeded $ applySubstitutions unifyingSubs (TypeVar "t-0") --Succeeded $ doSub unifyingSubs (TypeVar "t-0")
	where
		unifyingSubs = unify constraints

unify :: [TypeConstraint] -> [TypeConstraint]
unify [] = []
unify (t:rest)
	| nonDefVarSub t		= (t:(unify (map (subTC t) rest)))
	| defVarNonVar t 		= (t:(unify (map (subTC t) rest)))
	| nonVarAndVar t 		= unify ((flipTC t):rest)
	| twoMatrices t 		= unify $ (matrixDimSubs t) ++ rest
	| twoFuncs t 			= unify $ (funcSubs t) ++ rest
	| isIdentity t 			= unify rest
	| otherwise 			= error $ "no sub for " ++ show (t:rest)

matrixDimSubs :: TypeConstraint -> [TypeConstraint]
matrixDimSubs ((Matrix r1 c1), (Matrix r2 c2)) = [(r1, r2), (c1, c2)]

funcSubs :: TypeConstraint -> [TypeConstraint]
funcSubs ((Func a1 a2), (Func a3 a4)) = [(a1, a3), (a2, a4)]

applySubstitutions :: [TypeConstraint] -> Type -> Type
applySubstitutions [] t = t
applySubstitutions s t = case lookup t s of
	Just subForT -> applySubstitutions (delete (t, subForT) s) subForT
	Nothing -> continueSubList s t

continueSubList s (Matrix d1 d2) = Matrix (applySubstitutions s d1) (applySubstitutions s d2)
continueSubList s (Func t1 t2) = Func (applySubstitutions s t1) (applySubstitutions s t2)
continueSubList _ t = t

flipTC :: TypeConstraint -> TypeConstraint
flipTC (a, b) = (b, a)

subTC :: TypeConstraint -> TypeConstraint -> TypeConstraint
subTC s (t1, t2) = (sub s t1, sub s t2)

sub :: (Type, Type) -> Type -> Type
sub (t1, t2) other = if other == t1
	then t2
	else continueSub (t1, t2) other

continueSub s (Matrix d1 d2) = Matrix (sub s d1) (sub s d2)
continueSub s (Func t1 t2) = Func (sub s t1) (sub s t2)
continueSub _ t = t

nonDefVarSub :: TypeConstraint -> Bool
nonDefVarSub (t, _) = (not $ isDefTypeVar t) && (isTypeVar t)

defVarNonVar :: TypeConstraint -> Bool
defVarNonVar (t, k) = (isDefTypeVar t) && ((isDefTypeVar k) || (not $ isTypeVar k))

nonVarAndVar :: TypeConstraint -> Bool
nonVarAndVar (front, back) = (not $ isTypeVar front) && (isTypeVar back)

twoMatrices :: TypeConstraint -> Bool
twoMatrices ((Matrix _ _), (Matrix _ _)) = True
twoMatrices _ = False

twoFuncs :: TypeConstraint -> Bool
twoFuncs ((Func _ _), (Func _ _)) = True
twoFuncs _ = False

isIdentity :: TypeConstraint -> Bool
isIdentity (t1, t2) = t1 == t2