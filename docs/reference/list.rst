Lists
=====

For any :doc:`type <types>` ``T``,  ``List(T)`` is the type of *finite lists* with
elements of type ``T``.  A list is an arbitrary *sequence*; that is, a
list is a collection of ``T`` values where the order matters.

In general, lists are written with square brackets.

* The empty list is written ``[]``.
* A list with specific elements can be written like this: ``[1, 2, 3]``.
* An :doc:`ellipsis <ellipsis>` can be used to generate a range of
  elements.  For example,

    ::

       Disco> [1 .. 5]
       [1, 2, 3, 4, 5]
       Disco> [1, 3 .. 9]
       [1, 3, 5, 7, 9]

* :doc:`List comprehension <comprehension>` notation can also be used,
  for example:

    ::

       Disco> [ abs(x - 5) | x in [1 .. 10], x^2 > 5]
       [2, 1, 0, 1, 2, 3, 4, 5]

* The built-in ``list`` function can be used to convert other
  collections (*e.g.* :doc:`sets <set>` or :doc:`bags <bag>`) to lists:

    ::

       Disco> list({1, 2, 3})
       [1, 2, 3]
       Disco> import num
       Disco> list(factor(12))
       [2, 2, 3]

* The built-in `append` function can be used to append two lists.

    ::

       Disco> import list
       Disco> :type append
       append : List(a) × List(a) → List(a)
       Disco> append([1,2,3], [5,6])
       [1, 2, 3, 5, 6]

* Strings (written in double quotes) are really just lists of characters.

    ::

       Disco> |"hello"|
       5
       Disco> append("hello", "world")
       "helloworld"
       Disco> [(c,1) | c in "hello"]
       [('h', 1), ('e', 1), ('l', 1), ('l', 1), ('o', 1)]

* The order of elements in a list matters.  Two lists with elements in a
  different order, or different number of copies of the elements, are
  not the same.  For example,

    ::

       Disco> [1,2,3] == [1,3,2]
       F
       Disco> [1,2,3] == [1,1,2,3]
       F

* To check whether a list contains a given element, one can use the
  ``elem`` operator (also written ``∈``):

    ::

       Disco> 2 elem [1,2,3]
       T
       Disco> 5 elem [1,2,3]
       F
       Disco> 2 ∈ [1,2,3]
       T

Recursive list processing
-------------------------

Fundamentally, lists in Disco are built from two ingredients:

- The empty list ``[]``
- The "cons" operator ``::`` which adds one more element to the start
  of a list.  That is, ``x :: rest`` is a list which has a first element ``x``
  and where ``rest`` is the rest of the list.

For example, ``1 :: [2,3,4,5]`` represents adding the element ``1`` to
the beginning of the list ``[2,3,4,5]``; it is the same as
``[1,2,3,4,5]``.

::

   Disco> 1 :: [2,3,4,5]
   [1, 2, 3, 4, 5]

In fact, any list can be written using only ``[]`` and ``::``.

- The empty list is ``[]``;
- ``x :: []`` represents a list with one element;
- ``x :: (y :: [])`` is a list with two elements;
- and so on.

::

   Disco> 1 :: []
   [1]
   Disco> 1 :: (2 :: [])
   [1, 2]
   Disco> 1 :: (2 :: (3 :: []))
   [1, 2, 3]

We can write recursive functions to process lists by :doc:`pattern
matching <pattern>` on ``[]`` and ``::``.  For example, below is a
function to add all the elements of a list of natural numbers:

::

   sum : List(N) -> N
   sum([]) = 0
   sum(n :: ns) = n + sum(ns)
