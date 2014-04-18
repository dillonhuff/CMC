module LinearCode(
	LinearCode, name, isName) where

name str = Name str

isName :: LinearCode -> Bool
isName (Name _) = True
isName _ = False

data LinearCode
	= Name String
	deriving (Eq)

instance Show LinearCode where
	show = showCode