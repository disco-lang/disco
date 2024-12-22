Size
====

The *size* of a :doc:`collection <collections>` ``c`` (a :doc:`list
<list>`, :doc:`bag <bag>`, or :doc:`set <set>`) can be found using the
notation ``|c|``.  For example,

::

   Disco> |{1,2,3}|
   3
   Disco> |{1,2,3} union {2,3,4,4}|
   4
   Disco> |[2 .. 7]|
   6
   Disco> |bag([1,1,2,3])|
   4
