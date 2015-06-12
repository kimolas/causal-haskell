import Numeric.LinearAlgebra
import Graph
import Adjacency

main :: IO ()
main = do
  putStr . dispf 0 . edges'' . toAdjacency $ completeGraph [1..1000]
