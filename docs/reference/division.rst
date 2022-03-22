Division
========

:doc:`Fractional <fraction>` and :doc:`rational numbers <rational>`
can be divided using the ``/`` operator.  For example:

::

    Disco> 1 / 5
    1/5
    Disco> (2/3) / (7/5)
    10/21

It is not possible to divide :doc:`natural numbers <natural>` or
:doc:`integers <integer>` since those :doc:`numeric types
<numeric>` have no concept of fractions; however, in many
situations, if you divide natural numbers or integers they will
be automatically converted to fractionals or rationals as appropriate:

::

   Disco> :type 1 * (-2)
   1 * (-2) : ℤ
   Disco> :type 1 / (-2)
   1 / (-2) : ℚ

If you want only the *quotient* when dividing (*i.e.* the integer
number of times one thing fits into another, disregarding any
remainder), you can use :doc:`integer division <integerdiv>`.  If you
want the *remainder* instead, you can use :doc:`the mod operator <mod>`.
