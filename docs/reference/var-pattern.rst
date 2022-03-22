Variable pattern
================

A variable :doc:`pattern <pattern>` simply consists of a single
variable.  It always successfully matches any input; within the
corresponding clause, the input can be referred to by the variable
name.  For example,

::

   f(n) = 3n + 1

means "for *any* input to the function ``f``, which we will call
``n``, output the value which is one more than three times the input
``n``."

The variable defined by a variable pattern is *local* to the clause
and cannot be referenced anywhere else.  For example:

::

   Disco> f : N -> N
   Disco> f(n) = 3n+1
   Disco> f(3)
   10
   Disco> n
   Error: there is nothing named n.
   https://disco-lang.readthedocs.io/en/latest/reference/unbound.html

This also means multiple function definitions can use the same
variable name without interfering with one another at all.
