module DataFlowGraphTests(
	dataFlowGraphTests) where

import DataFlowGraph
import TestUtils

dataFlowGraphTests = do
	graphSizeTests

graphSizeTests = testFunction numNodes sizeTestCases

sizeTestCases =
	[emptyGraph
	,oneNode
	,severalNodes]

emptyGraph = (emptyDataFlowGraph, 0)

oneNode = (dataFlowGraph [(genData "N" 1 2, 1, [])], 1)

severalNodes = (dataFlowGraph
	[(genData "K" 3 4, 1, [20])
	,(genData "Ui" 7 6, 20, [])
	,(genData "Wqi" 87 123, 2, [1])], 3)