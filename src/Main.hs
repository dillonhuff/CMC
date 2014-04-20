module Main(main) where

import AnnotatedFunction
import Control.Monad
import ErrorHandling
import Expression
import LinearMatrixCode
import Parser
import System.Environment
import System.IO

main = do
	arguments <- getArgs
	let fileName = head arguments
	fileHandle <- openFile fileName ReadMode
	contents <- hGetContents fileHandle
	let parsedFunc = (parseFunction contents) >>= annotateFunc
	putStrLn $ show parsedFunc
	let lmc = liftM linearMatrixCode parsedFunc
	putStrLn $ show $ lmc
	case lmc of
		Failed errMsg -> displayError errMsg
		Succeeded linCode -> showSuccessAndWriteToFile $ scalarLoopFunction linCode

displayError errMsg = putStrLn errMsg

showSuccessAndWriteToFile scalarCode = do
	writeFile "CMC_out.c" (show scalarCode)
	putStrLn "Success!"