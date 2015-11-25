-- Haskell module for working with k-cores. 
-- Nicolas Kim, Department of Statistics, Carnegie Mellon University
-- July 9 2015

module Core where

import Adjacency
import qualified Data.List as L
import qualified Data.Map.Strict as M

-- The shell distribution of a given graph. 
-- shell :: (Ord a, Eq a) => Graph' a -> Map a [a]
-- shell gr@(Graph' ns es) = 

-- -- Slice a list. 
-- slice :: [a] -> [Int] -> [a]
-- slice xs = map (\x -> xs!!x)

-- The complete set of edges between nodes in a list. 13.27s
completeEdges :: (Ord a) => [a] -> [(a, a)]
completeEdges ns = [ (x, y) | x <- ns, y <- ns, x < y ]

-- Powerset of a list in constant time, due to Mark P Jones. 
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)

-- Creates a dictionary for the possible edge sets. 
powerMap :: [a] -> M.Map Int [[a]]
powerMap xs = L.foldl' f M.empty (powerSet xs)
  where
    f x y = M.insertWith (++) (length y) [y] x

-- List of all graphs on n nodes. Organized as a dictionary, with the key
-- being the number of edges in the graph. 
mGraphs :: (Ord a, Eq a) => [a] -> M.Map Int [Graph a]
mGraphs ns = M.map (map (Graph ns)) . powerMap $ completeEdges ns

-- Graph isomorphism checking (its complexity is unknown). The proper
-- approach would be to dynamically generate only unique graphs up to
-- isomorphism, but that's less straightforward. 
-- isomorphism :: (Ord a, Eq a) -> M.Map Int [Graph a] -> M.Map Int [Graph a]
-- isomorphism grs = 
