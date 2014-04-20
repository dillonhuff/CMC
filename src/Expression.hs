module Expression(
	Expression, Function,
	assign, operator, funcall,
	unaryOp, binaryOp, function,
	functionSpec, annotatedFunction,
	float, matrix, identifier, annotateFunc,
	typeOfExpr, checkFunctionTypes) where

import AnnotatedFunction
import Control.Monad
import Data.Tuple
import DataProperties
import ErrorHandling
import TypeSystem

data Function = FC Expression [[Expression]] [Expression] [Expression] [Expression]
	deriving (Eq, Show)

function :: Expression -> [Expression] -> [Expression] -> [Expression] -> Function
function name args body returnVals =
	FC name (replicate (length args) ([] :: [Expression])) args body returnVals

functionSpec name props args body returnVals = FC name props args body returnVals

data Expression
	= Identifier String
	| Funcall String
	| Assign Expression Expression
	| Matrix Int Int [Float]
	| UnaryOp Expression Expression
	| BinaryOp Expression Expression Expression
	| Op String
	deriving (Eq, Show)

assign :: Expression -> Expression -> Expression
assign e1 e2 = Assign e1 e2

identifier :: String -> Expression
identifier name = Identifier name

funcall :: String -> Expression
funcall name = Funcall name

operator name = Op name

unaryOp :: Expression -> Expression -> Expression
unaryOp op operand = UnaryOp op operand

binaryOp :: Expression -> Expression -> Expression -> Expression
binaryOp op operand1 operand2 =
	BinaryOp op operand1 operand2

matrix :: Int -> Int -> [Float] -> Expression
matrix rows cols vals = Matrix rows cols vals

-- Scalars are represented as 1 by 1 matrices
float :: Float -> Expression
float val = Matrix 1 1 [val]

-- Returns either an error or a list of all identifiers in the
-- function along with their types
checkFunctionTypes :: Function -> Error [(Expression, Type)]
checkFunctionTypes (FC name shapes args body returnVals) = outputTypes
	where
		inputTypes = makeInputVars shapes args
		outputTypes = foldM nextExprTypes inputTypes body

makeInputVars :: [[Expression]] -> [Expression] -> [(Expression, Type)]
makeInputVars props ids = zip ids (map idType properties)
	where
		properties = zip props ids

idType :: ([Expression], Expression) -> Type
idType (props, matName@(Identifier name)) = if (length props) > 1
	then error $ show matName ++ " has more than 1 shape" -- As more properties are added this will be changed
	else if length props == 0
		then genMatrix (name ++ "->row") (name ++ "->col")
		else specialMatrixDims (head props) matName

specialMatrixDims :: Expression -> Expression -> Type
specialMatrixDims (Identifier shapeName) (Identifier idName) =
	case shapeName of
		"Scalar" -> defMatrix 1 1
		"RowVector" -> leftDefMatrix 1 (idName ++ "->col")
		"ColumnVector" -> rightDefMatrix (idName ++ "->row") 1
		"UpperTriangular" -> genMatrix (idName ++ "->row") (idName ++ "->row")
		"LowerTriangular" -> genMatrix (idName ++ "->row") (idName ++ "->row")
		"Symmetric" -> genMatrix (idName ++ "->row") (idName ++ "->row")
		"General" -> genMatrix (idName ++ "->row") (idName ++ "->col")
		_ -> error $ shapeName ++ " is not a valid matrix shape"

-- TODO find better replacement for all of these liftM function calls
nextExprTypes :: [(Expression, Type)] -> Expression -> Error [(Expression, Type)]
nextExprTypes curIds (Assign (Identifier name) expr) = case lookup (Identifier name) curIds of
	Just val -> Failed $ name ++ " has already been assigned and cannot be re-computed"
	Nothing -> nextTypes
	where
		exprTypeAndConstraints = typeOfExpr curIds expr
		newIdType = liftM fst exprTypeAndConstraints
		newId = liftM swap $ errorTuple newIdType (Identifier name)
		newTypeConstrs = liftM snd exprTypeAndConstraints
		updatedIds = liftM (updateIds curIds) newTypeConstrs
		nextTypes = liftM2 (:) newId updatedIds
nextExprTypes _ expr = Failed $ show expr ++ " is not a properly formed assignment"

updateIds :: [(Expression, Type)] -> [TypeConstraint] -> [(Expression, Type)]
updateIds oldIds newConstraints = newIds
	where
		idNames = map fst oldIds
		idTypes = map snd oldIds
		unifyingSub = unify newConstraints
		newIds = zip idNames (map (applySubstitutions unifyingSub) idTypes)

valTypes :: [(Expression, Type)] -> [Expression] -> Error [(Expression, Type)]
valTypes _ _ = Succeeded []

getOutTypes :: [Expression] -> [(Expression, Type)] -> Error [Type]
getOutTypes _ _ = Succeeded []

-- Code to determine the type of an expression
typeOfExpr :: [(Expression, Type)] -> Expression -> Error (Type, [TypeConstraint])
typeOfExpr idTypes expr = errorTuple (computeType constraints) constraints
	where
		constraints = getExprConstraints (builtins ++ idTypes) "t-0" expr

builtins =
	[(Op "unary--", func (genMatrix "a" "b") (genMatrix "a" "b"))
	,(Op "'", func (genMatrix "a" "b") (genMatrix "b" "a"))
	,(Op "!", func (genMatrix "a" "a") (genMatrix "a" "a"))
	,(Op "+", func (genMatrix "a" "b") (func (genMatrix "a" "b") (genMatrix "a" "b")))
	,(Op "-", func (genMatrix "a" "b") (func (genMatrix "a" "b") (genMatrix "a" "b")))
	,(Op "*", func (genMatrix "a" "b") (func (genMatrix "b" "c") (genMatrix "a" "c")))
	,(Op ".*", func (defMatrix 1 1) (func (genMatrix "a" "b") (genMatrix "a" "b")))]

-- TODO: find more elegant way to deal with unary vs. binary '-'
getExprConstraints :: [(Expression, Type)] -> String -> Expression -> [TypeConstraint]
getExprConstraints _ tv (Matrix r c _) = [(typeVar tv, defMatrix r c)]
getExprConstraints context tv i@(Identifier _) = case lookup i context of
	Just t -> [(typeVar tv, t)]
	Nothing -> error $ "No such var as " ++ show i
getExprConstraints context tv (UnaryOp op arg) =
	[(opType, func argType resType)] ++ argConstraints
	where
		opType = case lookup (toUnaryForm op) context of
			Nothing -> error $ show op ++ " is not an operator"
			Just t -> uniqueTypeNames (tv ++ "-p") t
		argType = typeVar (tv ++ "0")
		resType = typeVar tv
		argConstraints = getExprConstraints context (tv ++ "0") arg
getExprConstraints context tv (BinaryOp op arg1 arg2) =
	[(opType, func arg1Type (func arg2Type resType))] ++ argConstraints
	where
		opType = case lookup op context of
			Nothing -> error $ show op ++ " is not an operator"
			Just t -> uniqueTypeNames (tv ++ "-p") t
		resType = typeVar tv
		arg1Type = typeVar (tv ++ "1")
		arg2Type = typeVar (tv ++ "2")
		argConstraints = (getExprConstraints context (tv ++ "1") arg1)
			++ (getExprConstraints context (tv ++ "2") arg2)

toUnaryForm :: Expression -> Expression
toUnaryForm (Op "-") = (Op "unary--")
toUnaryForm n = n

-- Code for converting the primitive function description output
-- by the parser to a function description that includes
-- information about special properties of results
-- computed by the function given to the compiler

annotateFunc :: Function -> Error AnnotatedFunction
annotateFunc f@(FC (Funcall name) specProps args body returnVals) =
	case anBody of
		Failed errMsg -> Failed errMsg
		Succeeded annotatedBody -> Succeeded $ annotatedFunction 
			name (anRVs args (fst annotatedBody)) (reverse $ snd annotatedBody) (anRVs returnVals (fst annotatedBody))
	where
		idTypes = checkFunctionTypes f
		argTypes = liftM (filter (\(ident, _) -> elem ident args)) idTypes
		anIds = liftM (annotatedIds specProps) argTypes
		anBody = liftM (annotateBody body) anIds

anRVs :: [Expression] -> [AExpr] -> [AExpr]
anRVs ids anIds = filter (\i -> elem (nameOf i) idNames) anIds
	where
		idNames = map (\(Identifier n) -> n) ids

annotateBody :: [Expression] -> [AExpr] -> ([AExpr], [AExpr])
annotateBody body initialIds = foldl annotateAssign (initialIds, []) body

annotateAssign :: ([AExpr], [AExpr]) -> Expression -> ([AExpr], [AExpr])
annotateAssign (ids, assigns) (Assign (Identifier name) arg) = (anIdent:ids, anAssign:assigns)
	where
		anArg = annotateExpr ids arg
		anIdent = aId name (shapeOf anArg)
		anAssign = aBinop "=" anIdent anArg (shapeOf anArg)

annotateExpr :: [AExpr] -> Expression -> AExpr
annotateExpr _ (Matrix r c vals) = aMat vals (dimsToShape r c)
annotateExpr ids (UnaryOp (Op name) arg) = aUnop name anArg (unopResultShape name (shapeOf anArg))
	where
		anArg = annotateExpr ids arg
annotateExpr ids (BinaryOp (Op name) arg1 arg2) = aBinop name anArg1 anArg2 resShape
	where
		anArg1 = annotateExpr ids arg1
		anArg2 = annotateExpr ids arg2
		resShape = binopResultShape name (shapeOf anArg1) (shapeOf anArg2)
annotateExpr ids (Identifier n) = case getIdShape n ids of
	Just shape -> aId n shape
	Nothing -> error $ "The impossible happened: After type checking " ++ n ++ " has no shape\n" ++ show ids

annotatedIds :: [[Expression]] -> [(Expression, Type)] -> [AExpr]
annotatedIds props idTypePairs = annotatedIdentifiers
	where
		idNames = map (\((Identifier n), _) -> n) idTypePairs
		idTypes = map (\(_, t) -> t) idTypePairs
		shapeNames = map (map (\(Identifier n) -> n)) props
		idShapes = zipWith makeShape shapeNames idTypes
		annotatedIdentifiers = zipWith aId idNames idShapes