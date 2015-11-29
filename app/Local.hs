-- Functions for local clustering

module Local where

import Adjacency
import Data.KMeans
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import Control.Monad
import Data.Maybe

-- Local edges to a node
local :: Ord a => Graph' a -> a -> [a]
local gr n = fromJust . M.lookup n $ edges' gr

-- Frequencies of local nodes
freq :: Ord a => Graph' a -> a -> M.Map a Int
freq gr n = L.foldl ins M.empty $ local gr n
  where
    ins m k = M.insertWith (+) k (1 :: Int) m

gr1 = toMap $ Graph [1..7] [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4), (4, 5), (5, 6), (5, 7), (6, 7)]

-- Using folds to create the neighborhood layer. Depends on lazy evaluation
-- to avoid actually constructing the entire array. 
nbhd :: Ord a => Graph' a -> a -> M.Map a Int
nbhd gr n = flip M.union locmap . flip M.intersection locmap . M.unionsWith (+) $ map (freq gr) loc
  where
    loc = local gr n
    locmap = M.fromList . zip loc $ repeat 0

-- Find the longest list in a list of lists
longest :: [[a]] -> [a]
longest = L.maximumBy (\x y -> compare (length x) (length y))

-- Infer connectivity of largest community
-- community :: Ord a => Graph' a -> a -> [[[Double]]]
community gr n = map fst . longest . kmeansGen (\x -> [fromIntegral $ snd x]) 2 . M.toList $ nbhd gr n
