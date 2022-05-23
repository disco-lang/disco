Binomial and multinomial coefficients
=====================================

The *binomial coefficient* :math:`\binom n k` represents the number of
different ways to choose a subset of size :math:`k`  out of a set of
size :math:`n`, and is in general given by the formula

:math:`\displaystyle \binom n k = \frac{n!}{k!(n-k)!}`

However, binomial coefficients can be computed more efficiently than
literally using the above formula with :doc:`factorial <factorial>`,
so Disco has special built-in support for computing them.  Since
:math:`\binom n k` is usually pronounced ":math:`n` choose :math:`k`",
the Disco syntax is ``n choose k``.  For example:

::

   Disco> 5 choose 2
   10
   Disco> 7 choose 0
   1
   Disco> 0 choose 0
   1
   Disco> 7 choose 8
   0
   Disco> 100 choose 23
   24865270306254660391200

Multinomial coefficients
------------------------

Disco also has support for *multinomial coefficients*.

:math:`\displaystyle \binom{n}{k_1 \quad k_2 \quad \dots \quad k_r} = \frac{n!}{k_1! k_2! \dots k_r! (n - k_1 - k_2 - \dots - k_r)!}`

is the number of ways to simultaneously choose subsets of size $k_1,
k_2, \dots, k_r$ out of a set of size $n$.  In Disco, a multinomial
coefficient results when the second argument to ``choose`` is a list
instead of a natural number:

::

   Disco> 10 choose 2
   45
   Disco> 10 choose [2]
   45
   Disco> 10 choose [2,3]
   2520
   Disco> 10 choose [2,3,5]
   2520
