Collection operations
=====================

The :doc:`Cartesian product <cp>` of two collections (:doc:`finite sets
<set>`, :doc:`lists <list>`, or :doc:`bags <bag>`) can be found
using the ``><`` operator.  See the :doc:`Cartiesian product <cp>`
page for more information.

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

We can also find the union of two bags, which works by adding multiplicities:

::

   Disco> bag [1,2,3] union bag [2,3,4]
   ⟅1, 2 # 2, 3 # 2, 4⟆

The *difference* of two sets or bags can be found using the difference
operator, written ``\``:

::

   Disco> {7 .. 12} \ {1 .. 10}
   {11, 12}
   Disco> bag [1,2,2,3] \ bag [2,3,4]
   ⟅1, 2⟆

You can check whether one set or bag is a subset of another using the
``subset`` operator (or the Unicode symbol ``⊆``):

::

   Disco> {2,3,4} subset {1 .. 10}
   T
   Disco> {7 .. 11} subset {1 .. 10}
   F
   Disco> (bag [1,2,2,3]) subset (bag [2,3,2,2,4,1])
   T

Note that Disco does not support the set complement operation, since
the complement of a finite set is infinite whenever the domain is
infinite.

