Type annotation
===============

A *type annotation* is similar to a :doc:`type signature <type-sig>`,
but placed on an :doc:`expression <expression>` instead of a top-level
:doc:`definition <definition>`.  Like a top-level type signature, a
type annotation says what type something should have.

For example, normally if we ask Disco for the type of ``3 + 5``, it
infers that it has type ``N``:

::

   Disco> :type 3 + 5
   3 + 5 : ℕ

However, if we want Disco to consider the ``3`` specifically to be the
*integer* ``3`` instead of just being a natural number, we can write this:

::

   Disco> :type (3 : Z) + 5
   (3 : ℤ) + 5 : ℤ

Now Disco infers that the whole thing has type ``Z``, since the sum of
an integer and a natural number must be an integer.

Adding type annotations is rarely necessary, but is occasionally
useful to be more specific about what type we would like something to have.
