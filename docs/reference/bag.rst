Bags
====

For any :doc:`type <types>` ``T``, ``Bag(T)`` is the type of *finite
bags* with elements of type ``T``.  A bag is like a :doc:`set` (and
unlike a :doc:`list`) in that it doesn't care about the order of the
elements; however, unlike a :doc:`set`, a bag cares *how many copies*
of each element there are.

* The built-in ``bag`` function can be used to convert other
  collections (*e.g.* :doc:`sets <set>` or :doc:`lists <list>`) to bags.

    ::

       Disco> bag([])
       ⟅⟆
       Disco> bag({1,2,3})
       ⟅1, 2, 3⟆
       Disco> bag("hello")
       ⟅'e', 'h', 'l' # 2, 'o'⟆

* Notice how single elements are simply listed by themselves, but
  elements occuring more than once are written with a ``#`` followed
  by a natural number.  You can write bags this way yourself:

    ::

       Disco> ⟅'a' # 2, 'b' # 3⟆
       ⟅'a' # 2, 'b' # 3⟆
       Disco> ⟅'a' # 2, 'b' # 1⟆
       ⟅'a' # 2, 'b'⟆
       Disco> ⟅'a' # 0, 'b' # 1⟆
       ⟅'b'⟆

* Of course, entering the ``⟅⟆`` characters can be difficult, so you
  can also just stick with entering bags via the ``bag`` function.

* One common place where bags show up is in the output of the ``factor``
  function (from the ``num`` standard library module) for representing
  the prime factorization of a natural number.  In a prime
  factorization, the *order* of the prime factors does not matter (since
  multiplication is commutative), but it matters how many copies there
  are of each factor.

    ::

       Disco> import num
       Disco> factor(10)
       ⟅2, 5⟆
       Disco> factor(12)
       ⟅2 # 2, 3⟆
       Disco> factor(64)
       ⟅2 # 6⟆

* Just as with :doc:`lists <list>` and :doc:`sets <set>`, an
  :doc:`ellipsis <ellipsis>` can be used to generate a range of
  elements in a bag.  For example,

    ::

       Disco> ⟅'a' .. 'e'⟆
       ⟅'a', 'b', 'c', 'd', 'e'⟆
       Disco> ⟅1, 3 .. 9⟆
       ⟅1, 3, 5, 7, 9⟆

* :doc:`Comprehension <comprehension>` notation can also be used,
  for example:

    ::

       Disco> ⟅ abs(x - 5) | x in ⟅1 .. 10⟆, x^2 > 5 ⟆
       ⟅0, 1 # 2, 2 # 2, 3, 4, 5⟆

* The order of elements in a bag does not matter, but the number of
  copies of each element does matter.  Two bags with the same elements
  in a different order are considered the same, whereas two bags with
  the same elements but a different number of copies of each are
  considered different.  For example,

    ::

       Disco> ⟅1,2,3⟆ == ⟅1,3,2⟆
       T
       Disco> ⟅1,2,3⟆ == ⟅1,1,2,3⟆
       F

* To check whether a bag contains a given element at least once, one can
  use the ``elem`` operator (also written ``∈``):

    ::

       Disco> 2 elem ⟅1,2,3⟆
       T
       Disco> 5 elem ⟅1,2,3⟆
       F
       Disco> 2 ∈ ⟅1,2,3⟆
       T

Bags support various operations, including :doc:`size <size>`,
:doc:`union <collection-ops>`, :doc:`intersection <collection-ops>`,
:doc:`difference <collection-ops>`, :doc:`subset <collection-ops>`, and :doc:`power set <power>`.

