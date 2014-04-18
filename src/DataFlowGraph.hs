module DataFlowGraph(
	DataFlowGraph,
	emptyDataFlowGraph,
	numNodes) where

import Data.Map
import DataProperties

emptyDataFlowGraph :: DataFlowGraph
emptyDataFlowGraph = (empty, empty)



type DataFlowGraph = (Map Vertex (DataFlowNode, [Vertex]), Map String Vertex)

numNodes :: DataFlowGraph -> Int
numNodes (m1, m2) = size m1

data DataFlowNode
	= Data String Shape
	| Op String Shape
	deriving (Eq, Show)