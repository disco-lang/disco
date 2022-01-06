Integer division
================

The *integer division* or *quotient* of two numbers, written ``//``,
is the integer number of times that one thing fits into another,
disregarding any :doc:`remainder <mod>`.  Alternatively, you can think
of it as :doc:`rounding down <round>` the result of the division to an
integer.  For example, ``11 // 2`` is ``5``, because ``2`` fits into
``11`` five times (with some left over).

::

   Disco> 11 // 2
   5
   Disco> 6 // 2
   3
   Disco> 6 // 7
   0
   Disco (-7) // 2
   -4

Formally, the result of ``//`` is defined in terms of the "Division
Algorithm": given a number :math:`n` and a divisor :math:`d`, the
quotient is the unique number :math:`q` such that :math:`n = qd + r`,
where :math:`0 \leq r < d`.
