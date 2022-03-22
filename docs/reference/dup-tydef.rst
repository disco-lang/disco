Duplicate definition for type T
===============================

This is very similar to :doc:`dup-def`, but for :doc:`type
definitions <tydef>` instead of variables.  For example,

::

   Disco> type H = N * N
   Disco> type H = Z + Q
   Error: duplicate definition for type H.

