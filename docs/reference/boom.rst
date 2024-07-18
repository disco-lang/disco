The Boom hierarchy
==================

.. warning::

   This section includes advanced information.  Don't worry about it
   unless you are interested in the abstract mathematics underlying
   the Disco language.

The :doc:`list <list>`, :doc:`bag <bag>`, and :doc:`set <set>`
:doc:`collection types <collections>` are all closely related; they
are all members of an abstract hierarchy known as the *Boom
hierarchy*.  The `Boom hierarchy`_ is named for its originator,
Hendrik Boom, but by a happy coincidence, *boom* is also the Dutch
word for "tree", which is where the hierarchy begins.

.. _`Boom hierarchy`: https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=dd3ed1c7d523781513f01d25e1be472de4279d72

Imagine binary trees where every node has exactly 0 or 2 children.
We can think of these trees as representing abstract syntax trees of
expressions with some binary operator (call it ``#``), where the
leaves of the tree store values of some type ``a``, and the internal
nodes are labelled with the operator ``#``.  For example, the
expression ``(2 # 3) # ((2 # 3) # 3)`` corresponds to the tree

::

          #
         / \
        /   \
       /     \
      #       #
     / \     / \
    2   3   #   3
           / \
          2   3

At this point, we don't make any assumptions about the binary operator
or what it means, so we have to write out all the parentheses in the
above expression.

* Suppose we now add a law that says the binary operator must be
  *associative*, that is, ``x # (y # z) = (x # y) # z`` for all ``x``,
  ``y``, and ``z``.  The *order* still matters---``x # y`` might not
  be the same as ``y # x``---but how we parenthesize things doesn't
  matter any more. So our expression from before can now unambiguously
  be written without parentheses, as ``2 # 3 # 2 # 3 # 3``. The only thing that matters anymore is the
  order of the values, so instead of trees we now have
  :doc:`lists <list>`.  Put another way, a list is a tree where we
  stop caring about how the branches were associated.

* Suppose we now add another law: the binary operator is *commutative*,
  that is, ``x # y = y # x``.  Now the order of the values does not
  matter, so our expression from before is equal to ``2 # 2 # 3 # 3 #
  3``. (For example, think of the addition operator.) However, it
  does still matter *how many* of each value we have, so instead of a
  list we have a multiset, *aka* :doc:`bag <bag>`.  Put another way,
  a bag is a list where we stopped caring about the order.

* We can finally add one more law: the binary operator must now be
  *idempotent*, that is, for any ``x`` it must be the case that ``x #
  x = x`` (for example, think of the ``max`` operator that returns
  the larger of its two arguments).  Now it no longer matters how
  many copies of each value we have, because they will all just
  combine into one; our example expression is now equal to just ``2 #
  3``.  So instead of a bag, we now have a :doc:`set <set>`.  Put
  another way, a set is a bag where we stopped caring about
  multiplicity.

We can use the ``list`` function to convert any collection to a list,
the ``bag`` function to convert any collection to a bag, and the
``set`` function to convert any collection to a set.  The behavior of
these conversion functions are summarized in the diagram below:

::

        ---- forget order ---->     ---- forget multiplicity ---->
   List                         Bag                                Set
        <--- sorted order -----     <------- one of each ---------

For example, to convert a list to a bag with ``bag``, we just forget
the order of the elements (but keep the multiplicities).  Likewise to
convert a bag to a set with ``set``, we just forget the
multiplicities.  And of course if we convert a list to a set, we
forget both the order and multiplicities all at once.

::

   Disco> bag "xzxy"
   ⟅'x' # 2, 'y', 'z'⟆
   Disco> set(⟅'x' # 2, 'y', 'z'⟆)
   {'x', 'y', 'z'}
   Disco> set ['x','z','x','y']
   {'x', 'y', 'z'}

In the other direction, in order to convert a set to a bag, we must
make up some standard multiplicities; one of each seems the most
reasonable choice.  To convert a bag to a list, we must pick a
standard order for the elements; we choose to sort them.

::

   Disco> bag({'x', 'y', 'z'})
   ⟅'x', 'y', 'z'⟆
   Disco> list(⟅'x' # 2, 'y', 'z'⟆
   "xxyz"
   Disco> list({'x', 'y', 'z'})
   "xyz"

This also means we can do some clever things by converting to some
other collection and then converting back.  For example, to sort a
list, we can just convert it to a bag and back.  Or, to sort while also
getting rid of duplicates, we can convert to a set and back.

::

   Disco> list(bag("xazybabxzcy"))
   "aabbcxxyyzz"
   Disco> list(set("xazybabxzcy"))
   "abcxyz"

We can also keep at most one copy of every element in a bag simply by
converting to a set and back.

On the other hand, converting from a set to a bag and back, or a set
to a list and back, has no effect: if we go left and then right in the
diagram above, we always end up with the same value we started with.

::

   Disco> set(list({2,5,7}))
   {2, 5, 7}

That's because moving from left to right in the above diagram means
losing information, while to move from right to left we have to make
up information.  Going right and then left, we may not make up the
same information that was lost.  However, going left and then right,
we will simply make up some information and then throw it away again.
