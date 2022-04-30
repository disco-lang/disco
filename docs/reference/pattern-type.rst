The pattern p is supposed to have type T, but instead...
========================================================

This is a similar sort of error as :doc:`notcon`, but concerns
:doc:`patterns <pattern>` instead of :doc:`expressions <expression>`.
On the one hand, disco can figure out what :doc:`type <type>` a
pattern should be based on the type of the :doc:`function <function>`.
On the other hand, the pattern itself may correspond to a different
type.  For example,

::

   Disco> :{
   Disco| f : N -> N
   Disco| f(x,y) = x + y
   Disco| :}
   Error: the pattern
     (x, y)
   is supposed to have type
     ℕ,
   but instead it has a pair type.

In this example, we have declared the type of ``f`` to be ``N -> N``,
that is, a function which takes a natural number as input and yields
another natural number as output.  However, we have used the pattern
``(x,y)`` for the input of ``f``, which looks like a value of a
:doc:`pair type <product-type>`.
