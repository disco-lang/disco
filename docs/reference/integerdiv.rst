Integer division
================

The *integer division* or *quotient* of two numbers, written ``//``,
is the result of the division *rounded down* to the nearest integer.
Intuitively, you can think of it as the number of times that one thing
fits into another, disregarding any :doc:`remainder <mod>`.  For
example, ``11 // 2`` is ``5``, because ``2`` fits into ``11`` five
times (with some left over).

::

   Disco> 11 // 2
   5
   Disco> 6 // 2
   3
   Disco> 6 // 7
   0
   Disco> (-7) // 2
   -4

In fact, ``//`` is simply defined in terms of regular :doc:`division
<division>` as well as the :doc:`floor operation <round>`:

::

   x // y = floor (x / y)

Although :doc:`dividing <division>` two integers using the usual ``/``
operator does not necessarily result in an integer, using integer
division does.  In particular, the integer division :doc:`operator
<operator>` can be given the types

::

   ~//~ : ℕ × ℕ → ℕ
   ~//~ : ℤ × ℤ → ℤ

Formally, the result of ``//`` is defined in terms of the "Division
Algorithm": given a number :math:`n` and a divisor :math:`d`, the
quotient ``n // d`` is the unique number :math:`q` such that :math:`n
= qd + r`, where :math:`0 \leq r < d`.
