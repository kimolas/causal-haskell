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
-- nbhd :: Ord a => Graph' a -> a -> M.Map a Int
-- nbhd gr n = do xs <- map (freq gr) <$> local gr n
--                M.unionsWith (+) <$> sequence xs

-- Using folds to create the neighborhood layer. Depends on lazy evaluation
-- to avoid actually constructing the entire array. 
nbhd :: Ord a => Graph' a -> a -> M.Map a Int
nbhd gr n = flip M.union locmap . flip M.intersection locmap . M.unionsWith (+) $ map (freq gr) loc
  where
    loc = local gr n
    locmap = M.fromList . zip loc $ repeat 0
                -- Want to use adjust/update to only add to the local nodes

-- Infer communities
community :: Ord a => Graph' a -> a -> [[[Double]]]
community gr n = kmeans 2 . map (\x -> [fromIntegral x]) . M.elems $ nbhd gr n
