-- | A thin layer on top of graphs from the @fgl@ package, which
--   allows dealing with vertices by label instead of by integer
--   @Node@ values.
module Graph where

import           Prelude                           hiding (map)
import qualified Prelude                           as P

import           Control.Arrow                     ((&&&))
import           Data.Map                          (Map, (!))
import qualified Data.Map                          as M
import           Data.Maybe                        (fromJust, isJust)
import           Data.Set                          (Set)
import qualified Data.Set                          as S
import           Data.Tuple                        (swap)

import qualified Data.Graph.Inductive.Graph        as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.DFS    as G (components,
                                                         condensation, topsort')

-- | Directed graphs, with vertices labelled by @a@ and unlabelled
--   edges.
data Graph a = G (Gr a ()) (Map a G.Node) (Map G.Node a)
  deriving Show

-- | Create a graph with the given set of vertices and directed edges.
mkGraph :: Ord a => Set a -> Set (a,a) -> Graph a
mkGraph vs es = G (G.mkGraph vs' es') a2n n2a
  where
    vs' = zip [0..] (S.toList vs)
    n2a = M.fromList vs'
    a2n = M.fromList . P.map swap $ vs'
    es' = P.map mkEdge (S.toList es)
    mkEdge (a1,a2) = (a2n ! a1, a2n ! a2, ())

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
delete :: Ord a => a -> Graph a -> Graph a
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
wccIDs (G g a2n n2a) = P.map S.fromList $ (P.map . P.map) (id &&& (n2a!)) (G.components g)

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
suc :: Ord a => Graph a -> a -> [a]
suc (G g a2n n2a) = P.map (n2a !) . G.suc g . (a2n !)

-- | Get a list of all the /predecessors/ of a given node in the
--   graph, /i.e./ all the nodes from which from the given node is
--   reachable by a directed path.  Does not include the given node
--   itself.
pre :: Ord a => Graph a -> a -> [a]
pre (G g a2n n2a) = P.map (n2a !) . G.pre g . (a2n !)

-- | Given a graph, return two mappings: the first maps each vertex to
--   its set of successors; the second maps each vertex to its set of
--   predecessors.  Equivalent to
--
--   > (M.fromList *** M.fromList) . unzip . map (\a -> ((a, suc g a), (a, pre g a))) . nodes $ g
--
--   but much more efficient.
cessors :: Ord a => Graph a -> (Map a (Set a), Map a (Set a))
cessors g@(G gg a2n n2a) = (succs, preds)
  where
    as = G.topsort' gg
    succs = foldr collectSuccs M.empty as  -- build successors map
    collectSuccs a m = M.insert a succsSet m
      where
        ss       = suc g a
        succsSet = S.fromList ss `S.union` (S.unions $ P.map (m!) ss)

    preds = foldr collectPreds M.empty (reverse as)  -- build predecessors map
    collectPreds a m = M.insert a predsSet m
      where
        ss       = pre g a
        predsSet = S.fromList ss `S.union` (S.unions $ P.map (m!) ss)
