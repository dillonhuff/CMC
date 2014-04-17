module ErrorHandling(
	Error(Succeeded, Failed),
	errorTuple,
	extractValue) where

data Error a = Succeeded a | Failed String
	deriving (Show)

instance Monad Error where
	return a = Succeeded a
	(Succeeded a) >>= f = f a
	(Failed errMsg) >>= f = (Failed errMsg)

extractValue :: Error a -> a
extractValue (Succeeded val) = val
extractValue (Failed errMsg) = error $ "Computation Failed: " ++ errMsg

errorTuple :: Error a -> b -> Error (a, b)
errorTuple (Failed errMsg) _ = Failed errMsg
errorTuple (Succeeded val) other = Succeeded (val, other)