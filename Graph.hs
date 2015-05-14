-- Graphical and Network Models in Haskell. 
-- Nicolas Kim
-- Department of Statistics, Carnegie Mellon University 
-- Distributed under the MIT License. 

import System.IO
import System.Random
import Control.Monad
-- import Control.Lens
import Control.Monad.Random
import qualified System.Random as R
import qualified Data.Map as M
import qualified Data.List as L
-- import qualified Numeric.LinearAlgebra as LA

-- Some examples of graphs. 
gr1 = Graph [1..3] [(1,2), (2,1), (1,3)]
gr2 = Graph [1..2] [(1,2), (2,1), (1,3)]
gr3 = completeGraph [1..10] 

-- Data structure for graphs and networks. 
data Graph a = Graph { nodes :: [a]
                     , edges :: [(a, a)]
                     } deriving (Show, Eq)

-- Data structure for network models. 
data Model = ERG { parameters :: [Double] } deriving (Show, Eq)

-- Makes an empty graph. 
emptyGraph :: Graph a
emptyGraph = Graph [] []

-- Makes a complete simple graph from a list of nodes. 
completeGraph :: (Ord a, Eq a) => [a] -> Graph a
completeGraph ns = Graph sns es
  where
    sns = L.sort ns
    es = completeEdges sns

-- Makes a valid graph object from a list of edges. 
toGraph :: (Ord a, Eq a) => [(a, a)] -> Graph a
toGraph es = Graph (extractNodes es) es

-- Checks if the edges contain defined nodes. 
validGraph :: (Eq a) => Graph a -> Bool
validGraph (Graph ns es) = subset (L.union (map fst es) (map snd es)) ns
  where
    subset xs ys = all (`elem` ys) xs

-- Removes redundant edges (e.g. (1,2) = (2,1)). 
redundant :: (Ord a, Eq a) => [(a, a)] -> [(a, a)]
redundant es = L.intersect es (map f es)
  where
    f (x, y) = (min x y, max x y)

-- Makes a graph valid (see the `validGraph` function). 
makeValid :: (Eq a) => Graph a -> Graph a
makeValid gr@(Graph ns es)
  | validGraph gr = gr
  | otherwise = Graph (L.union ens ns) es
    where
      ens = L.nub $ L.union (map fst es) (map snd es)

-- From a list of edges, give the sorted and pruned list of nodes
extractNodes :: (Ord a, Eq a) => [(a, a)] -> [a]
extractNodes es = L.nub $ L.sort allnodes
  where
    allnodes = (map fst es) ++ (map snd es)

-- The complete set of edges between nodes in a list. 
completeEdges :: (Ord a, Eq a) => [a] -> [(a, a)]
completeEdges ns = [ (x, y) | x <- ns, y <- ns, x < y ]

-- Add a set of nodes to a graph using their labels. 
addNodes :: (Ord a) => Graph a -> [a] -> Graph a
addNodes (Graph ns es) nns = Graph (L.union ns nns) es

-- Add a set of edges to a graph. 
addEdges :: (Ord a) => Graph a -> [(a, a)] -> Graph a
addEdges (Graph ns es) ees = Graph ns (L.union es ees)


-- -- Various graph statistics. 
-- Computes the number of nodes. 
nNode :: Graph a -> Int
nNode gr = length $ nodes gr

-- Computes the number of edges. 
nEdge :: Graph a -> Int
nEdge gr = length $ edges gr


-- -- Monad-based generation of random nodes and edges, i.e. random graphs.
-- Erdos-Renyi Model
-- Include or don't include an edge? 
pBool :: (RandomGen g) => Double -> Rand g Bool
pBool p = liftM (< p) $ getRandomR ((0, 1) :: (Double, Double))

-- Decide over a list of edges. 
pBools :: (RandomGen g) => [Double] -> Rand g [Bool]
pBools ps = sequence $ map pBool ps

-- Given a list of Bools and any other list, return the list subsetted by
-- the list of Bools. 
byBool :: [Bool] -> [a] -> [a]
byBool bs xs = [ snd zs | zs <- (zip bs xs), fst zs ]

-- Select elements from a list independently and with probabilities ps. 
setEdges :: (RandomGen g) => [a] -> [Double] -> Rand g [a]
setEdges xs ps = liftM (flip byBool xs) $ pBools ps

-- Generate an Erdos-Renyi random graph; nodes ns, and edge probabilities
-- ps. 
erdosGen :: (Ord a, RandomGen g) => [a] -> [Double] -> Rand g (Graph a)
erdosGen ns ps = liftM (Graph ns) $ setEdges (completeEdges ns) ps

-- Graphon Model: generates a w-random graph. 
graphonGen :: (Ord a, RandomGen g) => [a] -> (Double -> Double -> Double)
  -> Rand g (Graph a)
graphonGen ns w = liftM (Graph ns) es
  where
    n = length ns
    us = replicate n $ getRandomR ((0, 1) :: (Double, Double))
    ps = sequence [ liftM2 w x y | x <- us, y <- us ]
    es = (setEdges $ completeEdges ns) =<< ps

main = do
  -- values <- evalRandIO . erdosGen [1..100] $ repeat 1
  values <- evalRandIO $ graphonGen [1..100] (\x y -> 0.5)
  putStrLn (show values)
  putStrLn (show . length $ edges values)


-- IO helper. 
-- readInt :: IO Int
-- readInt = readLn

-- Diagnostics. 
-- main :: IO ()
-- main = do
--     n <- readInt
--     putStr . (\x -> "\nNumber of edges: "++x++"\n") . show . length . 
--     edges $ completeGraph [1..n]
