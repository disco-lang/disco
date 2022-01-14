Cyclic type definition for T
============================

This error occurs when one or more :doc:`type definitions <tydef>`
form a cycle.

Note that :doc:`recursive types <rec-type>`, types defined in terms of
themselves, are very much allowed (and useful)!  A "cyclic type" error
only occurs when a type is defined as being directly equal to itself.

For example:

::

   Disco> :{
   Disco| type A = B
   Disco| type B = C
   Disco| type C = A
   Disco| :}
   Error: cyclic type definition for A.
