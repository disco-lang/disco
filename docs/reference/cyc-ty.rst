Cyclic type definition for T
============================

This error occurs when one or more :doc:`type definitions <typedef>`
form a cycle.

Note that recursive types, *i.e.* types defined in terms of
themselves, are :doc:`very much allowed (and useful) <typedef>`!  A
"cyclic type" error only occurs when a type is defined as being
directly equal to itself.

For example:

::

   Disco> :{
   Disco| type A = B
   Disco| type B = C
   Disco| type C = A
   Disco| :}
   Error: cyclic type definition for A.
