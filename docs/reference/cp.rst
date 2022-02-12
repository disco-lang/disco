Cartesian product
=================

The *Cartesian product* operator, written ``><`` (or, using Unicode,
as ``⨯``), operates on two collections of the same type (either
:doc:`sets <set>`, :doc:`bags <bag>`, or :doc:`lists <list>`), and
forms the collection of all possible pairs with one element taken from
the first collection and the other from the second.

* On lists, the order matters: the resulting list has the first
  element of the first list matched with all elements of the second
  list, then the second element of the first list matched with all
  elements of the second list, and so on.

    ::

       Disco> [2,1,1] >< [6,7]
       [(2, 6), (2, 7), (1, 6), (1, 7), (1, 6), (1, 7)]

* On sets, we simply get the set of all unique pairs.

    ::

       Disco> {2,1,1} >< {6,7}
       {(1, 6), (1, 7), (2, 6), (2, 7)}

* The behavior of Cartesian product on bags is slightly less
  intuitive, but follows directly from the fact that taking the
  Cartesian product of two lists and then converting the result to a
  bag always yields the same result as first converting the two lists
  to bags and then taking the Cartesian product (although the latter
  can be more efficient).  That is, for all lists ``l1`` and ``l2``,
  ``bag(l1 >< l2) == bag(l1) >< bag(l2)``.

  In particular, if ``a`` is an element of bag ``A`` with a multiplicity
  of ``m``, and ``b`` is an element of bag ``B`` with a multiplicity of
  ``n``, then ``(a,b)`` is an element of ``A >< B`` with a
  multiplicity of ``m * n``.  In other words, we have ``m * n`` ways
  to form the pair ``(a,b)`` if we have ``m`` copies of ``a`` to
  choose from and ``n`` copies of ``b`` to choose from.

    ::

       Disco> bag([1,1,2,3]) >< bag([8,7,7,7])
       ⟅(1, 7) # 6, (1, 8) # 2, (2, 7) # 3, (2, 8), (3, 7) # 3, (3, 8)⟆

