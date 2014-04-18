module Main(main) where

import Expression
import Parser
import System.Environment
import System.IO

main = do
	arguments <- getArgs
	let fileName = head arguments
	fileHandle <- openFile fileName ReadMode
	contents <- hGetContents fileHandle
	putStrLn $ show $ (parseFunction contents) >>= annotateFunc