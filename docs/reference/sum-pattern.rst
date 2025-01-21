Sum patterns
============

A sum :doc:`pattern <pattern>` consists of a value of a :doc:`sum type
<sum-type>` used as a pattern, that is, either ``left(P)`` or
``right(P)``, where ``P`` is some pattern.  For example,

::

   f : (N + List(Char)) -> N
   f(left(n))    = n
   f(right(str)) = |str|

This is how we can write a function that takes an input with multiple
possibilities, and decides what to do with each possibility.

The arguments to ``left`` and ``right`` can themselves be any pattern,
not just variables.  For example,

::

   f : (N + Q) -> N
   f(left(2n+1)) = 17
   f(left(2n)) = 18
   f(right(q)) = abs(floor(q))
