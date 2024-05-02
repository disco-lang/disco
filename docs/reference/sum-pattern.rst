Sum patterns
============

A sum :doc:`pattern <pattern>` consists of an application of either
``left`` or ``right`` used as a pattern, where the argument to ``left`` or
``right`` can itself be any pattern.  The simplest kind
of sum pattern would just be ``left`` or ``right`` applied to :doc:`variables <var-pattern>`,
like

::

   f : (Z + Q) -> N
   f(left(z)) = abs(z)
   f(right(q)) = abs(floor(q))

The argument to ``left`` or ``right`` can itself be any pattern,
however, not just a variable.  For example,

::

   f : N + N -> N
   f(left(2n+1)) = 17
   f(left(2n)) = n + 1
   f(right(x)) = x
