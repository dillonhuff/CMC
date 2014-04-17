module Expression(
	Expression, Function,
	assign, operator, funcall,
	unaryOp, binaryOp, function,
	float, matrix, identifier,
	typeOfExpr, checkFunctionTypes) where

import Control.Monad
import Data.Tuple
import ErrorHandling
import TypeSystem

data Function = FC Expression [Expression] [Expression] [Expression]
	deriving (Eq, Show)

function :: Expression -> [Expression] -> [Expression] -> [Expression] -> Function
function name args body returnVals = FC name args body returnVals

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
-- function (including input identifiers) along with their types
checkFunctionTypes :: Function -> Error [(Expression, Type)]
checkFunctionTypes (FC name args body returnVals) = outputTypes
	where
		inputTypes = makeInputVars args
		outputTypes = foldM nextExprTypes inputTypes body

makeInputVars :: [Expression] -> [(Expression, Type)]
makeInputVars ids = zip ids (map idType ids)
	where
		idType (Identifier name) = genMatrix (name ++ "-row") (name ++ "-col")

nextExprTypes :: [(Expression, Type)] -> Expression -> Error [(Expression, Type)]
nextExprTypes curIds (Assign (Identifier name) expr) = nextTypes
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
		newIds = zip idNames (map (doSub newConstraints) idTypes)

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
			Just t -> t
		argType = typeVar (tv ++ "0")
		resType = typeVar tv
		argConstraints = getExprConstraints context (tv ++ "0") arg
getExprConstraints context tv (BinaryOp op arg1 arg2) =
	[(opType, func arg1Type (func arg2Type resType))] ++ argConstraints
	where
		opType = case lookup op context of
			Nothing -> error $ show op ++ " is not an operator"
			Just t -> t
		resType = typeVar tv
		arg1Type = typeVar (tv ++ "1")
		arg2Type = typeVar (tv ++ "2")
		argConstraints = (getExprConstraints context (tv ++ "1") arg1)
			++ (getExprConstraints context (tv ++ "2") arg2)

toUnaryForm :: Expression -> Expression
toUnaryForm (Op "-") = (Op "unary--")
toUnaryForm n = n