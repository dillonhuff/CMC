module TypeSystem(
	computeType,
	typeVar, defMatrix, genMatrix,
	leftDefMatrix, rightDefMatrix, func) where

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

isMatrix :: Type -> Bool
isMatrix (Matrix _ _) = True
isMatrix _ = False

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

computeType :: [TypeConstraint] -> Error Type
computeType constraints = Succeeded $ doSub unifyingSubs (TypeVar "t0")
	where
		unifyingSubs = unify constraints

type Sub = [(Type, Type)]

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

{-subMatrix :: Sub -> Type -> Type
subMatrix s m@(Matrix r c) = matrixDimensionSub s r c

matrixDimensionSub :: Sub -> Type -> Type -> Type
matrixDimensionSub s r c = Matrix newR newC
	where
		matrixSubs = filter (\(s1, s2) -> isMatrix s1) s
		matrixDimSubs = concat $ map matSubToDimSubs matrixSubs
		newR = case lookup r matrixDimSubs of
			Nothing -> r
			Just sVal -> sVal
		newC = case lookup c matrixDimSubs of
			Nothing -> c
			Just sVal -> sVal-}

matSubToDimSubs :: (Type, Type) -> Sub
matSubToDimSubs (Matrix r1 c1, Matrix r2 c2) = [(r1, r2), (c1, c2)]

nextSub :: (Type, Type) -> Sub
nextSub (TypeVar n, t) = if not (elem (TypeVar n) (var t))
	then [(TypeVar n, t)]
	else []
nextSub (Matrix v1 v2, Matrix v3 v4) = [(Matrix v1 v2, Matrix v3 v4)]
nextSub _ = []

nextTerms :: (Type, Type) -> [(Type, Type)]
nextTerms (TypeVar n1, TypeVar n2) = []
nextTerms (s, TypeVar n) = [(TypeVar n, s)]
nextTerms (Func t1 t2, Func t3 t4) = [(t1, t3), (t2, t4)]
nextTerms (Matrix r1 c1, Matrix r2 c2) = [(r1, r2), (c1, c2)]
nextTerms _ = []

var :: Type -> [Type]
var t@(TypeVar _) = [t]
var _ = []