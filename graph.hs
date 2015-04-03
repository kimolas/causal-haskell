-- (Kinda) Graphical Models in Haskell. 
-- (Also works for networks, sort of)
-- Nicolas Kim
-- Department of Statistics, Carnegie Mellon University 
-- 26 March 2015

import qualified Data.Map as Map
import qualified Data.List as List

-- let gr1 = Graph [1..3] [(1,2), (2,1), (1,3)]
-- let gr2 = Graph [1..2] [(1,2), (2,1), (1,3)]

data Graph = Graph { nodes :: [Int]
                   , edges :: [(Int, Int)]
                   } deriving (Show, Eq)

data Model = Model { statistics :: [Float] } deriving (Show, Eq)

-- Checks if the edges contain defined nodes. 
validGraph :: Graph -> Bool
validGraph (Graph ns es) = subset (List.union (map fst es) (map snd es)) ns
    where
      subset xs ys = all (`elem` ys) xs

-- Makes a graph valid (see the `validGraph` function). 
makeValid :: Graph -> Graph
makeValid gr@(Graph ns es)
  | validGraph gr = gr
  | otherwise = (Graph (List.union ens ns) es)
    where
      ens = List.nub $ List.union (map fst es) (map snd es)

-- Add a set of nodes to a graph using their labels. 
addNodes :: Graph -> [Int] -> Graph
addNodes (Graph ns es) nns = (Graph (List.union ns nns) es)

-- Add a set of edges to a graph. 
addEdges :: Graph -> [(Int, Int)] -> Graph
addEdges (Graph ns es) ees = (Graph ns (List.union es ees))

-- Model fitting. 
fitERG :: Graph -> Model
fitERG gr@(Graph ns es) = Model [ density ]
  where
    density = (/) (fromInteger $ length es)(fromInteger $ length ns)
