-- Graphical and Network Models in Haskell. 
-- Nicolas Kim
-- Department of Statistics, Carnegie Mellon University 
-- Distributed under the MIT License. 

import System.IO
import System.Random

import Adjacency
import Core

import Control.Monad
import Control.Monad.Random
import Control.Parallel (par, pseq)
-- import Control.Lens

import qualified Data.Map.Strict as M
import qualified Data.List as L

import Numeric.LinearAlgebra (dispf)

-- import qualified Numeric.LinearAlgebra as LA

-- Some examples of graphs. 
gr1 = Graph [1..3] [(1,2), (1,3), (2,3)] :: Graph Int
gr2 = Graph [1..2] [(1,2), (1,3), (2,3)] :: Graph Int
gr3 = completeGraph [1..10] :: Graph Int
gr4 = Graph [1..4] [(1,2), (3,4), (1,4)] :: Graph Int

kite = Graph [1..4] [(1,2), (1,3), (2,3), (2,4), (3,4)] :: Graph Int
wiki = Graph [1..6] [(1,2), (1,5), (2,5), (2,3), (3,4), (4,5), (4,6)] ::
  Graph Int
gr5 = Graph [1..3] [(1,2)] :: Graph Int
disconnected = Graph [1..4] [(1,2), (3,4)] :: Graph Int

-- Data structure for network models. 
-- data Model = ERG { parameters :: [Double] } deriving (Show, Eq)

-- Makes an empty graph. 
emptyGraph :: Graph a
emptyGraph = Graph [] []

-- Makes a complete simple graph from a list of nodes. 
completeGraph :: (Ord a, Eq a) => [a] -> Graph a
completeGraph ns = Graph sns es
  where
    sns = L.sort ns
    es = completeEdges sns

-- List of all possible graphs on n nodes. 
-- allGraphs :: (Ord a, Eq a) => [a] -> [Graph a] -> M.Map [Int] [Graph a]
-- allGraphs ns = L.foldl' () mapInit [1..n]
--   where
--     mapInit = M.fromList [(0, emptyGraph)]
--     n = (\x -> x*(x-1) `div` 2) $ length ns

-- Makes a valid graph object from a list of edges. 
toGraph :: (Ord a, Eq a) => [(a, a)] -> Graph a
toGraph es = Graph (extractNodes es) es

-- Checks if the edges contain defined nodes. 
validGraph :: (Eq a) => Graph a -> Bool
validGraph (Graph ns es) = subset (L.union (map fst es) (map snd es)) ns
  where
    subset xs ys = all (`elem` ys) xs

-- Removes redundant edges (e.g. (1,2) = (2,1)). 
redundant :: (Ord a, Eq a) => [(a, a)] -> [(a, a)]
redundant es = L.intersect es (map f es)
  where
    f (x, y) = (min x y, max x y)

-- Makes a graph valid (see the `validGraph` function). 
makeValid :: (Eq a) => Graph a -> Graph a
makeValid gr@(Graph ns es)
  | validGraph gr = gr
  | otherwise = Graph (L.union ens ns) es
    where
      ens = L.nub $ L.union (map fst es) (map snd es)

-- From a list of edges, give the sorted and pruned list of nodes
extractNodes :: (Ord a, Eq a) => [(a, a)] -> [a]
extractNodes es = L.nub $ L.sort allnodes
  where
    allnodes = (map fst es) ++ (map snd es)

-- The complete set of edges between nodes in a list. 13.27s
completeEdges :: (Ord a) => [a] -> [(a, a)]
completeEdges ns = [ (x, y) | x <- ns, y <- ns, x < y ]

-- The complete set of edges between nodes in a list. Nodes don't need to
-- be orderable. 22s
completeEdges' :: [a] -> [(a, a)]
completeEdges' ns = [ (fst x, fst y) | x <- zs, y <- zs, snd x < snd y ]
  where
    zs = zip ns [1..]

-- Add a set of nodes to a graph using their labels. 
addNodes :: (Ord a) => Graph a -> [a] -> Graph a
addNodes (Graph ns es) nns = Graph (L.union ns nns) es

-- Add a set of edges to a graph. 
addEdges :: (Ord a) => Graph a -> [(a, a)] -> Graph a
addEdges (Graph ns es) ees = Graph ns (L.union es ees)


-- -- Various graph statistics. 
-- Computes the number of nodes. 
nNode :: Graph a -> Int
nNode gr = length $ nodes gr

-- Computes the number of edges. 
nEdge :: Graph a -> Int
nEdge gr = length $ edges gr


-- -- Monad-based generation of random nodes and edges, i.e. random graphs.
-- Erdos-Renyi Model
-- Include or don't include an edge? 
pBool :: (RandomGen g) => Double -> Rand g Bool
pBool p = liftM (< p) $ getRandomR ((0, 1) :: (Double, Double))

-- Decide over a list of edges. 
pBools :: (RandomGen g) => [Double] -> Rand g [Bool]
pBools ps = sequence $ map pBool ps

-- Given a list of Bools and any other list, return the list subsetted by
-- the list of Bools. 
byBool :: [Bool] -> [a] -> [a]
byBool bs xs = [ snd zs | zs <- (zip bs xs), fst zs ]

-- Select elements from a list independently and with probabilities ps. 
setEdges :: (RandomGen g) => [a] -> [Double] -> Rand g [a]
setEdges xs ps = liftM (flip byBool xs) $ pBools ps

-- Generate an Erdos-Renyi random graph; nodes ns, and edge probabilities
-- ps. 
erdosGen :: (Ord a, RandomGen g) => [a] -> [Double] -> Rand g (Graph a)
erdosGen ns ps = liftM (Graph ns) $ setEdges (completeEdges ns) ps

-- Graphon Model: generates a w-random graph. 
graphonGen :: (Ord a, RandomGen g) => [a] -> (Double -> Double -> Double)
  -> Rand g (Graph a)
graphonGen ns w = liftM (Graph ns) es
  where
    us = liftM (take (length ns)) $ getRandomRs ((0, 1) :: (Double, Double))
    es = (liftM (applyUpper w) us) >>= (setEdges (completeEdges ns))

-- Graphon Model: generates a w-random graph. Parallel. 
graphonGen' :: (Ord a, RandomGen g) => [a] -> (Double -> Double -> Double)
  -> Rand g (Graph a)
graphonGen' ns w = liftM2 pseq (liftM2 par (liftM force es) (liftM force
                               es')) (liftM (Graph ns) (liftM2 (++) es
                               es'))
  where
    us = liftM (take (length ns)) $ getRandomRs ((0, 1) :: (Double, Double))
    cs = completeEdges ns
    n = length ns
    cshalf = splitAt (((n * (n-1))+2) `div` 4) cs
    es = (liftM (applyUpper w) us) >>= (setEdges (fst cshalf))
    es' = (liftM (applyUpper w) us) >>= (setEdges (snd cshalf))

-- From "Real World Haskell". Used in the parallel version of the code for
-- generating w-random graphs, `graphonGen'`. 
force :: [a] -> ()
force xs = go xs `pseq` ()
  where go (_:xs) = go xs
        go [] = 1

-- Apply a function to the upper triangle of an array. 
applyUpper :: (a -> a -> b) -> [a] -> [b]
applyUpper f xs = [ f (fst x) (fst y) | x <- zl, y <- zl, snd x < snd y ]
  where
    zl = zip xs ([1..] :: [Int])

sblock :: Double -> Double -> Double
sblock x y
  | x < 0.5 && y < 0.5 = 1
  | x < 0.5 || y < 0.5 = 0
  | otherwise          = 1

-- Need to create a show instance for this (or rewrite it). 
round' :: (RealFrac a, RealFrac b, Integral b) => Int -> a -> b
round' x = (/ 10^x) . round . (* 10^x)

main :: IO ()
main = do
    -- values <- evalRandIO $ graphonGen [1..10] (\x y -> (x+y)/2)
    let n = 30 :: Int
    values <- evalRandIO $ graphonGen [1..n] sblock
    -- let values = completeGraph [1..n]
    -- putStrLn . show $ laplacian values
    let spannum = spanTreeCount values
    putStrLn . (\x -> "Number of spanning trees: " ++ x) $ show spannum
    -- putStrLn . show $ spectralCluster' values
    putStrLn . (\x -> "Guess for optimal lambda: " ++ x) . show
      $ lambdaSelect values
    -- putStrLn . (\x -> "This is " ++ x ++ "% of the true value." ) . show
    --   . round' 4 . ((/) $ fromIntegral n^(n-2)) $ fromIntegral spannum
