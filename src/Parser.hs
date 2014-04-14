module Parser(
	parseAssignment) where

import ErrorHandling
import Expression
import Lexer
import Text.Parsec
import Text.Parsec.Expr

parseAssignment :: String -> Error Expression
parseAssignment str = (lexer str) >>= parseAsg

parseAsg :: [PosTok] -> Error Expression
parseAsg toks = case parse pAssign "Parser" toks of
	Left err -> Failed $ show err
	Right asg -> Succeeded asg

-- TODO: Add checks to make sure the matrix isn't ragged
makeMatrix :: [[Float]] -> Expression
makeMatrix elements = matrix rows cols flatElems
	where
		flatElems = concat elements
		rows = length elements
		cols = length $ head elements

pAssign = do
	name <- pIdentifier
	cmcTok equalsTok
	body <- pExpr
	cmcTok semicolonTok
	return $ assign name body

pIdentifier = do
	idTok <- identifierTok
	return $ identifier $ tokName idTok

pScalar = do
	numberToken <- scalarTok
	return $ float $ floatTokValue numberToken

pMatrix = do
	cmcTok lbracketTok
	values <- sepBy pMatrixRow (cmcTok semicolonTok)
	cmcTok rbracketTok
	return $ makeMatrix values

pMatrixRow = many1 pMatrixElem

pMatrixElem = pPosMatrixElem <|> pNegMatrixElem

pPosMatrixElem = do
	val <- scalarTok
	return $ floatTokValue val

pNegMatrixElem = do
	cmcTok (opTok "-")
	val <- scalarTok
	return $ (-1) * (floatTokValue val)

pExpr = buildExpressionParser table term

term = pParens pExpr
	<|> pIdentifier
	<|> pScalar
	<|> pMatrix

pParens e = do
	cmcTok lparenTok
	val <- e
	cmcTok rparenTok
	return val

table =
	[[postfixOp "!", postfixOp "'"]
	,[prefixOp "-"]
	,[infixOp "*", infixOp "/", infixOp ".*"]
	,[infixOp "+", infixOp "-"]]

prefixOp opStr = Prefix $ doUnoperator opStr
postfixOp opStr = Postfix $ doUnoperator opStr
infixOp opStr = Infix (doBinop opStr) AssocLeft

doUnoperator opStr = do
	opVal <- cmcTok (opTok opStr)
	return $ unaryOp (operator opStr)

doBinop opStr = do
	opVal <- cmcTok (opTok opStr)
	return $ binaryOp (operator opStr)

identifierTok :: (Monad m) => ParsecT [PosTok] u m PosTok
identifierTok = tokOfType isIdentifierTok

scalarTok :: (Monad m) => ParsecT [PosTok] u m PosTok
scalarTok = tokOfType isFloatTok

operatorTok :: (Monad m) => ParsecT [PosTok] u m PosTok
operatorTok = tokOfType isOperatorTok

tokOfType :: (Monad m) => (Tok -> Bool) -> ParsecT [PosTok] u m PosTok
tokOfType isTokOfType = tokenPrim show updatePos idTok
	where
		idTok pt = if isTokOfType (tok pt) then Just pt else Nothing

cmcTok :: (Monad m) => Tok -> ParsecT [PosTok] u m PosTok
cmcTok x = tokenPrim show updatePos testTok
	where
		testTok pt = if (tok pt) == x then Just pt else Nothing

updatePos :: SourcePos -> PosTok -> [PosTok] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position