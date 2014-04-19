module DataFlowGraph(
	DataFlowGraph,
	genData,
	dataFlowGraph,
	emptyDataFlowGraph,
	numNodes) where

import Data.Map as M
import DataProperties

dataFlowGraph :: [(DataFlowNode, Vertex, [Vertex])] -> DataFlowGraph
dataFlowGraph nodes = (vertexMap, identifierMap)
	where
		vertexList = Prelude.map (\(d, v, l) -> (v, (d, l))) nodes
		vertexMap = fromList vertexList
		identifierList = Prelude.filter (\(d, v, l) -> isId d) nodes
		identifierMap = fromList $ Prelude.map (\(d, v, l) -> (name d, v)) identifierList

emptyDataFlowGraph :: DataFlowGraph
emptyDataFlowGraph = (empty, empty)

type Vertex = Int

type DataFlowGraph = (Map Vertex (DataFlowNode, [Vertex]), Map String Vertex)

numNodes :: DataFlowGraph -> Int
numNodes (m1, m2) = size m1

dataNode :: String -> Shape -> DataFlowNode
dataNode name shape = Data name shape

data DataFlowNode
	= Data String Shape
	| Op String Shape
	deriving (Eq, Show)

isId :: DataFlowNode -> Bool
isId (Data name shape) = name /= ""
isId _ = False

name :: DataFlowNode -> String
name (Data n _) = n
name (Op n _) = n

genData :: String -> Int -> Int -> DataFlowNode
genData name r c = Data name (defGeneral r c)