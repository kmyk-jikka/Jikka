module Jikka.Common.Graph where

import Control.Monad
import Control.Monad.ST
import Data.List (nub)
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type Graph = V.Vector [Int]

makeReversedDigraph :: Graph -> Graph
makeReversedDigraph g = runST $ do
  let n = V.length g
  h <- MV.replicate n []
  forM_ [0 .. n - 1] $ \x -> do
    forM_ (g V.! x) $ \y -> do
      MV.modify h (x :) y
  V.freeze h

makeInducedDigraph :: Graph -> V.Vector Int -> Graph
makeInducedDigraph g f = runST $ do
  let n = V.length g
  let k = if V.null f then 0 else V.maximum f + 1
  h <- MV.replicate k []
  forM_ [0 .. n - 1] $ \x -> do
    forM_ (g V.! x) $ \y -> do
      MV.modify h ((f V.! y) :) (f V.! x)
  forM_ [0 .. k - 1] $ \a -> do
    MV.modify h nub a
  V.freeze h

-- | `decomposeToStronglyConnectedComponents` does SCC in \(O(V + E)\) using Kosaraju's algorithm.
-- It takes a digraph \(G = (V, E)\) as an adjacent list \(g : V \to V^{\lt \omega}\), and returns an mapping \(f : V \to V'\) for the SCC DAG \(G' = (V', E')\).
-- The indices of vertices of the SCC DAG are topologically sorted.
decomposeToStronglyConnectedComponents :: Graph -> V.Vector Int
decomposeToStronglyConnectedComponents g = runST $ do
  let n = V.length g
  let unless' used x f = do
        usedX <- MV.read used x
        unless usedX $ do
          f
  -- The first DFS
  let order = topologicalSort g
  -- DFS on the reversed graph
  let gRev = makeReversedDigraph g
  componentOf <- MV.replicate n (-1)
  size <- newSTRef 0
  used <- MV.replicate n False
  let go x = do
        MV.write used x True
        forM_ (gRev V.! x) $ \y -> do
          unless' used y $ do
            go y
  V.forM_ order $ \x -> do
    unless' used x $ do
      go x
      modifySTRef' size succ
  V.freeze componentOf

-- | `topologicalSort` does topological sort in \(O(V + E)\) using Tarjan's algorithm.
-- The input is an adjacent list of a DAG.
topologicalSort :: Graph -> V.Vector Int
topologicalSort g = runST $ do
  let n = V.length g
  let unless' used x f = do
        usedX <- MV.read used x
        unless usedX $ do
          f
  order <- newSTRef []
  used <- MV.replicate n False
  let go x = do
        MV.write used x True
        forM_ (g V.! x) $ \y -> do
          unless' used y $ do
            go y
        modifySTRef' order (x :)
  forM_ [0 .. n - 1] $ \x -> do
    unless' used x $ do
      go x
  V.fromList <$> readSTRef order
