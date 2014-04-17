module TypeSystem(
	Type, computeType, TypeConstraint, typeConstraint,
	typeVar, defMatrix, genMatrix, doSub, flipSub, unify,
	leftDefMatrix, rightDefMatrix, func, uniqueTypeNames) where

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
computeType constraints = Succeeded $ doSub unifyingSubs (TypeVar "t-0")
	where
		unifyingSubs = unify constraints

type Sub = [(Type, Type)]

flipSub :: Sub -> Sub
flipSub sub = map (\(x, y) -> (y, x)) sub

unify :: [(Type, Type)] -> Sub
unify [] = []
unify (t:rest) = if (fst t == snd t)
	then unify rest
	else ((nextSub t) ++ (unify $ doSubList (nextSub t) $ (nextTerms t) ++ rest))

doSubList :: Sub -> [(Type, Type)] -> [(Type, Type)]
doSubList s pairs = map (\(x, y) -> (doSub s x, doSub s y)) pairs

doSub :: Sub -> Type -> Type
doSub s m@(Matrix r c) = Matrix (doSub s r) (doSub s c)
doSub s t@(TypeVar _) = case lookup t s of
	Just subsT -> doSub (delete (t, subsT) s) subsT
	Nothing -> t
doSub s t = t

matSubToDimSubs :: (Type, Type) -> Sub
matSubToDimSubs (Matrix r1 c1, Matrix r2 c2) = [(r1, r2), (c1, c2)]

nextSub :: (Type, Type) -> Sub
nextSub (TypeVar n, t) = if not (elem (TypeVar n) (var t))
	then if not (isDefTypeVar (TypeVar n))
		then [(TypeVar n, t)]
		else []
	else []
nextSub (Matrix v1 v2, Matrix v3 v4) = []
nextSub _ = []

nextTerms :: (Type, Type) -> [(Type, Type)]
nextTerms (TypeVar n, TypeVar m) = if isDefTypeVar (TypeVar n)
	then [(TypeVar m, TypeVar n)]
	else []
nextTerms (Dimension n, Dimension m) = if n == m
	then []
	else error $ "Dimensions " ++ show n ++ " and " ++ show m ++ " don't match"
nextTerms (s, TypeVar n) = [(TypeVar n, s)]
nextTerms (Func t1 t2, Func t3 t4) = [(t1, t3), (t2, t4)]
nextTerms (Matrix r1 c1, Matrix r2 c2) = [(r1, r2), (c1, c2)]
nextTerms (s, t) = if (not $ isTypeVar s) && (not $ isTypeVar t)
	then error $ "Cannot substitute type " ++ show t ++ " for type " ++ show s
	else []

var :: Type -> [Type]
var t@(TypeVar _) = [t]
var (Matrix t1 t2) = (var t1) ++ (var t2)
var (Func t1 t2) = (var t1) ++ (var t2)
var _ = []