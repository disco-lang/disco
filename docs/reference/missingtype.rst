The definition of x must have an accompanying type signature
============================================================

In Disco, you are not allowed to define a variable by simply saying
``x = ...``.  You must also specify the :doc:`type <types>` of a variable
by placing a :doc:`type signature <type-sig>` before it, like this:

::

   x : N   -- a type signature for x
   x = 5   -- the definition of x
