module Main(main) where

import AnnotatedFunction
import Asmx86
import Control.Monad
import ErrorHandling
import Expression
import LinearMatrixCode
import Parser
import System.Environment
import System.IO

-- Temporary test main to help with figuring out how assembly generation is formatted
main = do
	arguments <- getArgs
	let outFileName = head arguments
	writeFile (outFileName ++ ".s") (toAsm "noWayFunc")

{-main = do
	arguments <- getArgs
	let fileName = head arguments
	fileHandle <- openFile fileName ReadMode
	contents <- hGetContents fileHandle
	let parsedFunc = parseFunction contents
	putStrLn $ show parsedFunc
	let annotatedFunc = parsedFunc >>= annotateFunc
	putStrLn $ show annotatedFunc
	let lmc = liftM linearMatrixCode annotatedFunc
	putStrLn $ show $ lmc
	case lmc of
		Failed errMsg -> displayError errMsg
		Succeeded linCode -> showSuccessAndWriteToFile $ scalarLoopFunction linCode-}

displayError errMsg = putStrLn errMsg

showSuccessAndWriteToFile scalarCode = do
	writeFile "CMC_out.c" (show scalarCode)
	putStrLn "Success!"