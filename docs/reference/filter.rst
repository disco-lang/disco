Filtering collections with ``filter``
=====================================

There is a special built-in function called ``filter`` which can be
used to keep only some of the values of a collection while discarding
others.  It can have any of the following types:

::

   filter : (a → Bool) × List(a) → List(a)
   filter : (a → Bool) × Bag(a) → Bag(a)
   filter : (a → Bool) × Set(a) → Set(a)

In other words, ``filter`` takes two arguments: the first is a
:doc:`function <function>` of some arbitrary type ``a → Bool`` (*i.e.* a
"predicate" on the type ``a``), and the second is a collection
(:doc:`list <list>`, :doc:`bag <bag>`, or :doc:`set <set>`) containing values of type
``a``.  ``filter`` then applies the function to every element in the
collection, and keeps only those values for which the function returns
true.

For example, we can use ``filter`` to keep only the even numbers in
set:

::

   Disco> import num
   Disco> filter(even, {1 .. 7})
   {2, 4, 6}

We can also write our own :doc:`anonymous function <anonymous-func>`
to express any predicate we want:

::

   Disco> filter(\n. (n == 1) or (n // 2 > 1), {1 .. 7})
   {1, 4, 5, 6, 7}

We can also apply ``filter`` to a list or bag:

::

   Disco> import num
   Disco> filter(isPrime, [0 .. 30])
   [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
   Disco> each(\x. x + 1, factor(60))
   ⟅3 # 2, 4, 6⟆

Note that it is always possible to use a :doc:`comprehension <comprehension>` instead
of ``filter``, if you prefer.  For example, instead of writing
``filter(even, {1 .. 7})`` we could write

::

   Disco> {x | x in {1 .. 7}, even(x)}
   {2, 4, 6}

