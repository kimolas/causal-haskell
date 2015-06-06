-- A lot of this code game from the blog of Nicolas Favre-Felix. 
module Svd where

import Numeric.LinearAlgebra 

compress :: Int -> Matrix Double -> Matrix Double
compress k m = u_k <> sigma_k <> v_k where
	(u,sigma,v) = svd m			-- get SVD
	sigma_k = (takeColumns k . takeRows k) $ diag sigma	-- keep k values of Σ
	u_k = takeColumns k u				-- keep k columns of U
	v_k = takeRows k $ trans v			-- keep k rows of v


-- I wrote the code below. 


