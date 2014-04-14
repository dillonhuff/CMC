module Expression(
	Expression,
	assign, operator,
	unaryOp, binaryOp,
	float, matrix, identifier,
	typeOfExpr) where

import ErrorHandling
import TypeSystem

data Expression
	= Identifier String
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

-- Code to determine the type of an expression
typeOfExpr :: Expression -> Error Type
typeOfExpr expr = computeType constraints
	where
		constraints = getExprConstraints builtins "t-0" expr

builtins =
	[(Op "-", func (genMatrix "a" "b") (genMatrix "a" "b"))
	,(Op "'", func (genMatrix "a" "b") (genMatrix "b" "a"))
	,(Op "!", func (genMatrix "a" "a") (genMatrix "a" "a"))
	,(Op "+", func (genMatrix "a" "b") (func (genMatrix "a" "b") (genMatrix "a" "b")))
	,(Op "-", func (genMatrix "a" "b") (func (genMatrix "a" "b") (genMatrix "a" "b")))]

getExprConstraints :: [(Expression, Type)] -> String -> Expression -> [TypeConstraint]
getExprConstraints _ tv (Matrix r c _) = [(typeVar tv, defMatrix r c)]
getExprConstraints context tv (UnaryOp op arg) =
	[(opType, func argType resType)] ++ argConstraints
	where
		opType = case lookup op context of
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