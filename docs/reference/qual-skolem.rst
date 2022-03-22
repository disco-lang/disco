Type variable x represents any type, so we cannot assume...
===========================================================

A :doc:`polymorphic function <polymorphism>` has to be able to work
for *any* input type. Thus, it cannot assume that input values of a
polymorphic type support any operations in particular.  For example,
the type of the function ``h`` below claims it works for any type
``a`` at all, but the implementation of ``h`` uses subtraction (which
does not actually work for any type):

::

   h : a -> a
   h(x) = x - 3
