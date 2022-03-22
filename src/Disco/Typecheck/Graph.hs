{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Typecheck.Graph
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A thin layer on top of graphs from the @fgl@ package, which
--   allows dealing with vertices by label instead of by integer
--   @Node@ values.
-----------------------------------------------------------------------------

module Disco.Typecheck.Graph where

import           Prelude                           hiding (map, (<>))
import qualified Prelude                           as P

import           Control.Arrow                     ((&&&))
import           Data.Map                          (Map)
import qualified Data.Map                          as M
import           Data.Maybe                        (fromJust, isJust, mapMaybe)
import           Data.Set                          (Set)
import qualified Data.Set                          as S
import           Data.Tuple                        (swap)

import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.DFS    as G (components,
                                                         condensation, topsort')

import           Disco.Pretty
import           Disco.Util                        ((!))

-- | Directed graphs, with vertices labelled by @a@ and unlabelled
--   edges.
data Graph a = G (Gr a ()) (Map a G.Node) (Map G.Node a)
  deriving Show

instance Pretty a => Pretty (Graph a) where
  pretty (G g _ _) = parens (prettyVertices <> ", " <> prettyEdges)
    -- (V = {(0, x), (1, N)}, E = {0 -> 1, 2 -> 3})
    where
      vs = G.labNodes g
      es = G.labEdges g

      prettyVertex (n,a) = parens (text (show n) <> ", " <> pretty a)
      prettyVertices = "V = " <> braces (intercalate "," (P.map prettyVertex vs))
      prettyEdge (v1,v2,_) = text (show v1) <+> "->" <+> text (show v2)
      prettyEdges = "E = " <> braces (intercalate "," (P.map prettyEdge es))

-- | Create a graph with the given set of vertices and directed edges.
--   If any edges refer to vertices that are not in the given vertex
--   set, they will simply be dropped.
mkGraph :: (Show a, Ord a) => Set a -> Set (a,a) -> Graph a
mkGraph vs es = G (G.mkGraph vs' es') a2n n2a
  where
    vs' = zip [0..] (S.toList vs)
    n2a = M.fromList vs'
    a2n = M.fromList . P.map swap $ vs'
    es' = mapMaybe mkEdge (S.toList es)
    mkEdge (a1,a2) = (,,) <$> M.lookup a1 a2n <*> M.lookup a2 a2n <*> pure ()

-- | Return the set of vertices (nodes) of a graph.
nodes :: Graph a -> Set a
nodes (G _ m _) = M.keysSet m

-- | Return the set of directed edges of a graph.
edges :: Ord a => Graph a -> Set (a,a)
edges (G g _ m) = S.fromList $ P.map (\(n1,n2,()) -> (m ! n1, m ! n2)) (G.labEdges g)

-- | Map a function over all the vertices of a graph.  @Graph@ is not
--   a @Functor@ instance because of the @Ord@ constraint on @b@.
map :: Ord b => (a -> b) -> Graph a -> Graph b
map f (G g m1 m2) = G (G.nmap f g) (M.mapKeys f m1) (M.map f m2)

-- | Delete a vertex.
delete :: (Show a, Ord a) => a -> Graph a -> Graph a
delete a (G g a2n n2a) = G (G.delNode n g) (M.delete a a2n) (M.delete n n2a)
  where
    n = a2n ! a

-- | The @condensation@ of a graph is the graph of its strongly
--   connected components, /i.e./ each strongly connected component is
--   compressed to a single node, labelled by the set of vertices in
--   the component.  There is an edge from component A to component B
--   in the condensed graph iff there is an edge from any vertex in
--   component A to any vertex in component B in the original graph.
condensation :: Ord a => Graph a -> Graph (Set a)
condensation (G g _ n2a) = G g' as2n n2as
  where
    g' = G.nmap (S.fromList . P.map (n2a !)) (G.condensation g)
    vs' = G.labNodes g'
    n2as = M.fromList vs'
    as2n = M.fromList . P.map swap $ vs'

-- | Get a list of the weakly connected components of a graph,
--   providing the set of vertices in each.  Equivalently, return the
--   strongly connected components of the graph when considered as an
--   undirected graph.
wcc :: Ord a => Graph a -> [Set a]
wcc = P.map (S.map snd) . wccIDs

wccIDs :: Ord a => Graph a -> [Set (G.Node, a)]
wccIDs (G g _a2n n2a) = P.map (S.fromList . P.map (id &&& (n2a !))) (G.components g)

-- | Do a topological sort on a DAG.
topsort :: Graph a -> [a]
topsort (G g _a2n _n2a) = G.topsort' g

-- | A miscellaneous utility function to turn a @Graph Maybe@ into a
--   @Maybe Graph@: the result is @Just@ iff all the vertices in the
--   input graph are.
sequenceGraph :: Ord a => Graph (Maybe a) -> Maybe (Graph a)
sequenceGraph g = case all isJust (nodes g) of
  False -> Nothing
  True  -> Just $ map fromJust g

-- | Get a list of all the /successors/ of a given node in the graph,
--   /i.e./ all the nodes reachable from the given node by a directed
--   path.  Does not include the given node itself.
suc :: (Show a, Ord a) => Graph a -> a -> [a]
suc (G g a2n n2a) = P.map (n2a !) . G.suc g . (a2n !)

-- | Get a list of all the /predecessors/ of a given node in the
--   graph, /i.e./ all the nodes from which from the given node is
--   reachable by a directed path.  Does not include the given node
--   itself.
pre :: (Show a, Ord a) => Graph a -> a -> [a]
pre (G g a2n n2a) = P.map (n2a !) . G.pre g . (a2n !)

-- | Given a graph, return two mappings: the first maps each vertex to
--   its set of successors; the second maps each vertex to its set of
--   predecessors.  Equivalent to
--
--   > (M.fromList *** M.fromList) . unzip . map (\a -> ((a, suc g a), (a, pre g a))) . nodes $ g
--
--   but much more efficient.
cessors :: (Show a, Ord a) => Graph a -> (Map a (Set a), Map a (Set a))
cessors g@(G gg _ _) = (succs, preds)
  where
    as = G.topsort' gg
    succs = foldr collectSuccs M.empty as  -- build successors map
    collectSuccs a m = M.insert a succsSet m
      where
        ss       = suc g a
        succsSet = S.fromList ss `S.union` S.unions (P.map (m !) ss)

    preds = foldr collectPreds M.empty (reverse as)  -- build predecessors map
    collectPreds a m = M.insert a predsSet m
      where
        ss       = pre g a
        predsSet = S.fromList ss `S.union` S.unions (P.map (m !) ss)
