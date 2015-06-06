-- Haskell code for navigating the interface between lists of edges and
-- adjacency matrices. 

module Adjacency where

import Numeric.LinearAlgebra

-- Data structure for graphs and networks. 
data Graph a = Graph { nodes :: [a]
                     , edges :: [(a, a)]
                     } deriving (Show, Eq)

-- This function was written by Nicolas Favre-Felix. It returns an
-- approximation of the given matrix, based on the SVD. 
compress :: Int -> Matrix Double -> Matrix Double
compress k m = u_k <> sigma_k <> v_k where
	(u,sigma,v) = svd m			-- get SVD
	sigma_k = (takeColumns k . takeRows k) $ diag sigma	-- keep k values of Σ
	u_k = takeColumns k u				-- keep k columns of U
	v_k = takeRows k $ trans v			-- keep k rows of v

