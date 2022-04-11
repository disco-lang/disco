Pattern p contains duplicate variable x
=======================================

A :doc:`pattern <pattern>` is not allowed to contain the same variable
more than once.  For example, the following definition is not allowed,
because the pattern ``(x,x)`` contains two occurrences of the variable ``x``.

::

   f :: N*N -> N
   f(x,x) = 3
   f(x,y) = 7

If you want to define a function which returns ``3`` whenever its two
arguments are equal, and ``7`` otherwise, you could define it like
this, using a :doc:`case expression <case>`

::

   f :: N*N -> N
   f(x,y) = {? 3 if x == y, 7 otherwise ?}
