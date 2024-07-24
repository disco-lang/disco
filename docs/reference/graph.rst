Graphs
======

Disco has a built-in type of (directed, unweighted) *graphs*.
``Graph(v)`` is the type of directed graphs with vertices labelled by
values from the type ``v``.

* We can use ``summary : Graph(v) -> Map(v, Set(v))`` turn turn a graph into
  a :doc:`map <map>` associating each vertex with the set of its
  (outgoing) neighbors.

* The empty graph, with no vertices and no edges, is written ``emptyGraph``.

* The simplest kind of nonempty graph we can make is a graph with a
  single vertex and no edges, using the function ``vertex : v ->
  Graph(v)``.

    ::

       Disco> :type vertex(1)
       vertex(1) : Graph(ℕ)
       Disco> summary(vertex(1))
       map({(1, {})})

* We can *union* (aka *overlay*) two graphs using ``overlay :
  Graph(v) * Graph(v) -> Graph(v)``.  We can also abbreviate ``overlay`` by
  ``+``.  The resulting graph has all the vertices and all the edges
  from either input graph.

    ::

       Disco> summary(vertex(1) + vertex(2))
       map({(1, {}), (2, {})})
       Disco> summary((vertex(1) + vertex(2)) + (vertex(2) + vertex(3)))
       map({(1, {}), (2, {}), (3, {})})

* We can *connect* two graphs using ``connect : Graph(v) * Graph(v) ->
  Graph(v)``, which we can also abbreviate by ``*``.  The resulting
  graph is the ``overlay`` of the two graphs, plus a directed edge
  pointing from each vertex in the first graph to each vertex in the
  second graph.

    ::

       Disco> summary(vertex(1) * vertex(2))  -- two vertices and a single edge
       map({(1, {2}), (2, {})})
       Disco> summary(vertex(1) * vertex(2) + vertex(2) * vertex(1))  -- edges in both directions
       map({(1, {2}), (2, {1})})
       Disco> summary((vertex(1) + vertex(2)) * vertex(3))
       map({(1, {3}), (2, {3}), (3, {})})

As a more extended example, here is how you could build path, cycle,
and complete graphs:

::

   path : N -> Graph(N)
   path(0) = emptyGraph
   path(1) = vertex(0)
   path(n) = vertex(n.-1) * vertex(n.-2) + path(n.-1)

   cycle : N -> Graph(N)
   cycle(0) = emptyGraph
   cycle(n) = path(n) + (vertex(0) * vertex(n.-1))

   uConnect : Graph(a) * Graph(a) -> Graph(a)
   uConnect (g,h) = g * h + h * g

   complete : N -> Graph(N)
   complete(0) = emptyGraph
   complete(1) = vertex(0)
   complete(n) = uConnect(vertex(n.-1), complete(n.-1))

::

   Disco> summary(cycle(4))
   map({(0, {3}), (1, {0}), (2, {1}), (3, {2})})
   Disco> summary(complete(4))
   map({(0, {1, 2, 3}), (1, {0, 2, 3}), (2, {0, 1, 3}), (3, {0, 1, 2})})

The Algebra of Graphs
---------------------

.. warning::

   This section includes advanced information.  Don't worry about it
   unless you are interested in the abstract mathematics underlying
   the Disco language.

The operators Disco has for building graphs (``vertex``, ``overlay``,
and ``connect``) are based on a theory called the *algebra of
graphs*.  You can `read the 2017 paper about it here`_.

.. _`read the 2017 paper about it here`: https://github.com/snowleopard/alga-paper/releases/download/final/algebraic-graphs.pdf
