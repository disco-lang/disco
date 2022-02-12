Set operations
==============

The :doc:`Cartesian product <cp>` of two :doc:`finite sets <set>` can be found
using the ``><`` operator.

::

   Disco> {1,2,3} >< {'x','y'}
   {(1, 'x'), (1, 'y'), (2, 'x'), (2, 'y'), (3, 'x'), (3, 'y')}

The *union* or *intersection* of two :doc:`finite sets <set>` can be found using
the ``union`` and ``intersect`` operators, or using the Unicode
notation ``∪`` and ``∩``.

::

   Disco> {1,2,3} union {2,3,4}
   {1, 2, 3, 4}
   Disco> {1,2,3} intersect {2,3,4}
   {2, 3}

The *difference* of two sets can be found using the set difference
operator, written ``\``:

::

   Disco> {7 .. 12} \ {1 .. 10}
   {11, 12}

You can check whether one set is a subset of another using the
``subset`` operator (or the Unicode symbol ``⊆``):

::

   Disco> {2,3,4} subset {1 .. 10}
   true
   Disco> {7 .. 11} subset {1 .. 10}
   false
