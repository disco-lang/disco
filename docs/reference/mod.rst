Modulo
======

The ``mod`` operator is used to give the *remainder* when one number
is :doc:`divided by <division>` another.

For example, ``11 mod 2`` is ``1``, because ``2`` fits into ``11`` five
times, with a remainder of 1; ``11 mod 4`` is ``3``, because dividing
``11`` by ``4`` leaves a remainder of ``3``.

::

   Disco> 11 mod 2
   1
   Disco> 11 mod 4
   3
   Disco> 6 mod 2
   0
   Disco> 6 mod 7
   6
   Disco> (-7) mod 2
   1

Formally, the result of ``mod`` is defined in terms of the "Division
Algorithm": given a number :math:`n` and a positive divisor :math:`d`, the
remainder ``n mod d`` is the unique number :math:`r` such that :math:`n
= qd + r`, where :math:`0 \leq r < d` and :math:`q` is the
:doc:`quotient <integerdiv>`.  (For negative divisors, we
instead require :math:`d < r \leq 0`.)
