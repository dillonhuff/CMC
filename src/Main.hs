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
	putStrLn $ showLinMatCode $ liftM linearMatrixCode parsedFunc

showLinMatCode code = case code of
	Failed errMsg -> errMsg
	Succeeded linCode -> show linCode