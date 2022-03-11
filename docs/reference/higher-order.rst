Higher-order functions
======================

A *higher-order* function is a :doc:`function <function>` which takes
other functions as input.  For example:

::

   twice : (N -> N) * N -> N
   twice(f, x) = f(f(x))

The ``twice`` function takes a function on natural numbers ``f``,
along with a natural number ``n``, as input, and calls ``f`` twice on
``n``.
