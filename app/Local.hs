-- Functions for local clustering

module Local where

import Adjacency
import Data.KMeans
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import Control.Monad

-- Frequencies of local nodes
freq :: Ord a => Graph' a -> a -> Maybe (M.Map a Int)
freq gr n = case loc of
              Just l  -> Just $ L.foldl ins M.empty l
              Nothing -> Nothing
  where
    loc = M.lookup n $ edges' gr
    ins m k = M.insertWith (+) k (1 :: Int) m

-- Using folds to create the neighborhood layers. Depends on lazy
-- evaluation to avoid actually constructing the entire array. 

