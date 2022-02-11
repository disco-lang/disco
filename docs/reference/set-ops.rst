Set operations
==============

The *union* or *intersection* of two :doc:`finite sets <set>` can be found using
the ``union`` and ``intersect`` operators, or using the Unicode
notation ``∪`` and ``∩``.

::

   Disco> {1,2,3} union {2,3,4}
   {1, 2, 3, 4}
   Disco> {1,2,3} intersect {2,3,4}
   {2, 3}

You can check whether one set is a subset of another using the
``subset`` operator (or the Unicode symbol ``⊆``):

::

   Disco> {2,3,4} subset {1 .. 10}
   true
   Disco> {7 .. 11} subset {1 .. 10}
   false
