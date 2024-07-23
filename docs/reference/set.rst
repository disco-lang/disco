Sets
====

For any :doc:`type <types>` ``T``,  ``Set(T)`` is the type of *finite sets* with
elements of type ``T``.

* The empty set is written ``{}``.
* A set with specific elements can be written like this: ``{1, 2, 3}``.
* An :doc:`ellipsis <ellipsis>` can be used to generate a range of
  elements.  For example,

    ::

       Disco> {1 .. 5}
       {1, 2, 3, 4, 5}
       Disco> {1, 3 .. 9}
       {1, 3, 5, 7, 9}

* :doc:`Set comprehension <comprehension>` notation can also be used,
  for example:

    ::

       Disco> {x^2 + 1 | x in {1 .. 10}, x > 4}
       {26, 37, 50, 65, 82, 101}
* The built-in ``set`` function can be used to convert other
  collections (*e.g.* :doc:`lists <list>`) to sets:

    ::

       Disco> set([1,2,3,2,3])
       {1, 2, 3}
       Disco> set("hello")
       {'e', 'h', 'l', 'o'}

The order of elements in a set does not matter, nor does the number of
copies of an element.  For example,

::

   Disco> {3,3,1,2} == {1,1,2,2,3,3}
   true
   Disco> {3, 3, 1, 2}
   {1, 2, 3}

To check whether a set contains a given element, one can use the
``elem`` operator (also written ``∈``):

::

   Disco> 2 elem {1,2,3}
   T
   Disco> 5 elem {1,2,3}
   F
   Disco> 2 ∈ {1,2,3}
   T

Sets support various operations, including :doc:`size <size>`,
:doc:`union <collection-ops>`, :doc:`intersection <collection-ops>`,
:doc:`difference <collection-ops>`, :doc:`subset <collection-ops>`, and :doc:`power set <power>`.
