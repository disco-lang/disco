Mapping over collections with ``each``
======================================

There is a special built-in function called ``each`` which can be used
to apply a function to every value of a collection.  It can have any
of the following types:

::

   each : (a → b) × List(a) → List(b)
   each : (a → b) × Bag(a) → Bag(b)
   each : (a → b) × Set(a) → Set(b)

In other words, ``each`` takes two arguments: the first is a
:doc:`function <function>`
of some arbitrary type ``a → b``, and the second is a collection
(:doc:`list <list>`, :doc:`bag <bag>`, or :doc:`set <set>`) containing values of type
``a``.  ``each`` then applies the function to every element in the
collection, returning the same kind of collection but now full of
``b`` values instead of ``a``. (``a`` and ``b`` *can* be different
types, but they do not have to be.)

For example, we can use ``each`` to take the :doc:`absolute value
<abs>` of every integer in a set, or multiply every value in a set by
10:

::

   Disco> each(abs, {-3 .. 2})
   {0, 1, 2, 3}
   Disco> each(\x. x*10, {1,2,3,7})
   {10, 20, 30, 70}

Notice how in the second example above we use an :doc:`anonymous
function <anonymous-func>` as the first argument to ``each``.
We can also apply ``each`` to a list or bag:

::

   Disco> each(\x. x // 2, [0 .. 6])
   [0, 0, 1, 1, 2, 2, 3]
   Disco> import num
   Disco> each(\x. x + 1, factor(60))
   ⟅3 # 2, 4, 6⟆

Note that it is always possible to use a :doc:`comprehension <comprehension>` instead
of ``each``, if you prefer.  For example, instead of writing
``each(\x. x * 10, {1,2,3,7})`` we could write

::

   Disco> {x*10 | x in {1,2,3,7}}
   {10, 20, 30, 70}

However, ``each`` is more fundamental (in fact, comprehensions are
implemented in terms of ``each``) and in some cases may be more
convenient.
