Divisibility testing
====================

We can test whether one number evenly divides another using the
``divides`` operator.  In particular, ``a divides b`` is true if there
exists an :doc:`integer <integer>` ``k`` such that ``b == k*a``.  For
example:

::

   Disco> 3 divides 6
   true
   Disco> 6 divides 3
   false
   Disco> 3 divides (-6)
   true
   Disco> 5 divides 5
   true
   Disco> 4 divides 6
   false
   Disco> 0 divides 10
   false
   Disco> 10 divides 0
   true
   Disco> 0 divides 0
   true
   Disco> 1/2 divides 3/2
   true
