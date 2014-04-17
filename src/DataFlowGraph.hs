module DataFlowGraph(
	DataFlowGraph,
	emptyDataFlowGraph) where

import Data.Map
import DataProperties

emptyDataFlowGraph :: DataFlowGraph
emptyDataFlowGraph = (empty, empty)

type DataFlowGraph = (Map Int DataFlowNode, Map String Int)

data DataFlowNode
	= Data String Shape [Int]
	| Op String Shape [Int]
	deriving (Eq, Show)