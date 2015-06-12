-- Haskell code for navigating the interface between lists of edges and
-- adjacency matrices. 

module Adjacency where

import Numeric.LinearAlgebra
import qualified Data.Map.Strict as M
import qualified Data.List as L

-- Data structure for graphs and networks. Just a list of edges. 
data Graph a = Graph { nodes :: [a]
                     , edges :: [(a, a)]
                     } deriving (Show, Eq)

data Graph' a = Graph' { nodes' :: [a]
                       , edges' :: M.Map a [(a, a)]
                       } deriving (Show, Eq)

data Graph'' = Graph'' { nodes'' :: [Int]
                       , edges'' :: Matrix Double
                       } deriving (Show, Eq)

-- Converts the edge list to a map structure. Takes up more space, but it's
-- faster for looking up what else is connected to a particular node. 
toMap :: (Ord a, Eq a) => Graph a -> Graph' a
toMap (Graph ns es) = Graph' ns es'
  where
    fInsert o m x = M.insertWith (++) (o x) [x] m
    es' = L.foldl' (fInsert snd) (L.foldl' (fInsert fst) M.empty es) es

-- Creates a (fst/snd)-biased dictionary for edges. 
toMap' :: (Ord a, Eq a) => ((a, a) -> a) -> Graph a -> Graph' a
toMap' f (Graph ns es) = Graph' ns es'
  where
    fInsert o m x = M.insertWith (++) (o x) [x] m
    es' = L.foldl' (fInsert f) M.empty es

-- Converts an edge map to an adjacency matrix structure. 
-- toAdjacency :: Graph' a -> Graph''
-- toAdjacency (Graph' ns es) = Graph'' ns m
--   where
--     n = length ns
--     m = (n >< n) . 

-- Given a list of edges which are all connected to a node, generates
-- a list of 0's and 1's denoting membership. 
toList :: (Ord a, Eq a) => [a] -> [(a,a)] -> [a]
toList (n:ns) es
  | fst e  = 
  | snd e  = 
  | True   = 

-- SVD approximation to a graph (uses k-element SVD of the adjacency
-- matrix). 
svdGraph :: Int -> Graph'' -> Graph''
svdGraph k (Graph'' ns es) = Graph'' ns $ compress k es

-- This function was written by Nicolas Favre-Felix. It returns an
-- approximation of the given matrix, by taking only the k largest entries
-- of Sigma. 
compress :: Int -> Matrix Double -> Matrix Double
compress k m = u_k <> sigma_k <> v_k where
	(u,sigma,v) = svd m			-- get SVD
	sigma_k = (takeColumns k . takeRows k) $ diag sigma	-- keep k values of Σ
	u_k = takeColumns k u				-- keep k columns of U
	v_k = takeRows k $ trans v			-- keep k rows of v

