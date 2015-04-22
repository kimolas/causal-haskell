-- Graphical and Network Models in Haskell. 
-- Nicolas Kim
-- Department of Statistics, Carnegie Mellon University 

import qualified Data.Map as M
import qualified Data.List as L

-- Some examples of graphs. 
gr1 = Graph [1..3] [(1,2), (2,1), (1,3)]
gr2 = Graph [1..2] [(1,2), (2,1), (1,3)]
gr3 = Graph [1..10] 


data Graph = Graph { nodes :: [Int]
                   , edges :: [(Int, Int)]
                   } deriving (Show, Eq)

data Statistic = Statistic { values :: [Double] } deriving (Show, Eq)

data Model = ERG { parameters :: [Double] } deriving (Show, Eq)

-- Makes an empty graph. 
emptyGraph :: Graph
emptyGraph = Graph [] []

-- Makes a complete graph from a list of nodes. 
completeGraph :: [Int] -> Graph
completeGraph ns = Graph ns (redundant es)
  where
    es = [ (x, y) | x <- ns, y <- ns ]

-- Makes a valid graph object from a list of edges. 
toGraph :: [(Int, Int)] -> Graph
toGraph es = Graph (extractNodes es) es

-- Checks if the edges contain defined nodes. 
validGraph :: Graph -> Bool
validGraph (Graph ns es) = subset (L.union (map fst es) (map snd es)) ns
  where
    subset xs ys = all (`elem` ys) xs

-- Removes redundant edges (e.g. (1,2) = (2,1)). 
redundant :: [(Int, Int)] -> [(Int, Int)]
redundant es = L.intersect es (map f es)
  where
    f (x, y) = (min x y, max x y)

-- Makes a graph valid (see the `validGraph` function). 
makeValid :: Graph -> Graph
makeValid gr@(Graph ns es)
  | validGraph gr = gr
  | otherwise = Graph (L.union ens ns) es
    where
      ens = L.nub $ L.union (map fst es) (map snd es)

-- From a list of edges, give the sorted and pruned list of nodes
extractNodes :: [(Int, Int)] -> [Int]
extractNodes es = L.nub $ L.sort allnodes
  where
    allnodes = (map fst es) ++ (map snd es)

-- Add a set of nodes to a graph using their labels. 
addNodes :: Graph -> [Int] -> Graph
addNodes (Graph ns es) nns = Graph (L.union ns nns) es

-- Add a set of edges to a graph. 
addEdges :: Graph -> [(Int, Int)] -> Graph
addEdges (Graph ns es) ees = Graph ns (L.union es ees)


-- Various graph statistics. 
-- Computes the graph density. 
density :: Graph -> Statistic
density gr@(Graph ns es) = Statistic [ dense ]
  where
    dense = (/) (fromIntegral $ length es) (fromIntegral $ length ns)
