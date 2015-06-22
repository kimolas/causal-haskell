-- Haskell code for navigating the interface between lists of edges and
-- adjacency matrices. 

module Adjacency where

import Numeric.LinearAlgebra hiding (fromList)
import qualified Numeric.LinearAlgebra as LA (fromList)
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.KMeans

-- Data structure for graphs and networks. Just a list of edges. 
data Graph a = Graph { nodes :: [a]
                     , edges :: [(a, a)]
                     } deriving (Show, Eq)

data Graph' a = Graph' { nodes' :: [a]
                       , edges' :: M.Map a [(a, a)]
                       } deriving (Show, Eq)

data Graph'' a = Graph'' { nodes'' :: [a]
                         , edges'' :: Matrix Double
                         } deriving (Show, Eq)

-- Converts the edge list to a map structure. Takes up more space, but it's
-- faster for looking up what else is connected to a particular node. 
toMap :: (Ord a, Eq a) => Graph a -> Graph' a
toMap (Graph ns es) = Graph' ns es'
  where
    fInsert o m x = M.insertWith (++) (o x) [x] m
    iMap = M.fromList $ map (\x -> (x, [])) ns
    es' = L.foldl' (fInsert snd) (L.foldl' (fInsert fst) iMap es) es

-- Converts the edge list to a map structure. Takes up more space, but it's
-- faster for looking up what else is connected to a particular node. 
toMap' :: (Ord a, Eq a) => Graph a -> Graph' a
toMap' gr@(Graph ns es) = Graph' ns es'
  where
    fInsert o o' m x = M.insertWith (++) (o x) [o' x] m
    iMap = M.fromList $ map (\x -> (x, [])) ns
    es' = L.foldl' (fInsert snd fst) (L.foldl' (fInsert fst snd) iMap es) es


-- Converts a graph (edge list) to an adjacency matrix structure. 
toAdjacency :: (Ord a, Eq a) => Graph a -> Graph'' a
toAdjacency gr@(Graph ns es) = Graph'' ns m
  where
    n = length ns
    m = (n >< n) . concat . map (to01List ns) $ map ((M.!) . edges' $ toMap' gr) ns

-- Converts a graph (edge list) to an adjacency matrix structure. About 1/3
-- slower than `toAdjacency`. 
toAdjacency' :: (Ord a, Eq a) => Graph a -> Graph'' a
toAdjacency' gr@(Graph ns es) = Graph'' ns m
  where
    n = length ns
    m = (n >< n) . M.foldl' (++) [] $ M.map (to01List ns) (edges' $ toMap' gr)

-- Given a list of edges which are all connected to a node, generates
-- a list of 0's and 1's denoting membership. 
to01List ::  (Eq a) => [a] -> [a] -> [Double]
to01List ns es = map (boolTo01 . flip elem es) ns

-- Converts False/True to 0/1. 
boolTo01 :: Bool -> Double
boolTo01 b
  | b    = 1
  | True = 0

-- SVD approximation to a graph (uses k-element SVD of the adjacency
-- matrix). In reality we need the SVD of XY^T, but that will come later. 
svdGraph :: Int -> Graph'' a -> Graph'' a
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

-- Constructs the graph Laplacian. 
laplacian :: (Ord a, Eq a) => Graph a -> Matrix Double
laplacian gr = (diag $ LA.fromList counts) - (edges'' $ toAdjacency gr)
  where
    counts = M.foldl (\x y -> x ++ [fromIntegral $ length y]) [] . edges'
             $ toMap gr

-- Spanning tree counter; uses Kirchhoff's Theorem. 
spanTreeCount :: (Ord a, Eq a) => Graph a -> Integer
spanTreeCount gr@(Graph ns es) = round . det . (takeColumns n' . takeRows
                                 n') $ laplacian gr
  where
    n' = length ns - 1
