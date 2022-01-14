Duplicate type signature for x
==============================

This error message is caused by multiple :doc:`type signatures
<type-sig>` for the same variable.  It does not matter if the types
are the same or different; there can only be one type signature per
variable.

::

   Disco> :{
   Disco| x : N
   Disco| x : N
   Disco| :}
   Error: duplicate type signature for x.

If this is unexpected, check that you did not misspell a variable name
so it accidentally has the same name as another variable.
