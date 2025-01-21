Divisibility testing
====================

We can test whether one number evenly divides another using the
``divides`` operator.  In particular, ``a divides b`` is true if there
exists an :doc:`integer <integer>` ``k`` such that ``b == k*a``.  For
example:

::

   Disco> 3 divides 6
   T
   Disco> 6 divides 3
   F
   Disco> 3 divides (-6)
   T
   Disco> 5 divides 5
   T
   Disco> 4 divides 6
   F
   Disco> 0 divides 10
   F
   Disco> 10 divides 0
   T
   Disco> 0 divides 0
   T
   Disco> 1/2 divides 3/2
   T
