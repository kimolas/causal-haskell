-- Functions for local clustering

module Local where

import Adjacency
import Data.KMeans
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import Control.Monad

-- Local edges to a node
local :: Ord a => Graph' a -> a -> Maybe [a]
local gr n = M.lookup n $ edges' gr

-- Frequencies of local nodes
freq :: Ord a => Graph' a -> a -> Maybe (M.Map a Int)
freq gr n = L.foldl ins M.empty <$> local gr n
  where
    ins m k = M.insertWith (+) k (1 :: Int) m

gr1 = toMap $ Graph [1..7] [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4), (4, 5), (5, 6), (5, 7), (6, 7)]

-- Using folds to create the neighborhood layer. Depends on lazy evaluation
-- to avoid actually constructing the entire array. 
nbhd :: Ord a => Graph' a -> a -> Maybe (M.Map a Int)
nbhd gr n = case maps of
              Just xs -> M.unionsWith (+) <$> sequence xs
              Nothing -> Nothing
  where
    maps = map (freq gr) <$> local gr n

-- Infer communities

