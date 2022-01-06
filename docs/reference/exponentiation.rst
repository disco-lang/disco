Exponentiation
==============

The ``^`` operator is used to raise one number to the power of
another.

::

   Disco> 2 ^ 5
   32
   Disco> 2 ^ 0
   1
   Disco> 2 ^ (-5)
   1/32
   Disco> (-3) ^ (-5)
   -1/243

If the exponent is a :doc:`natural number <natural>`, the result will
have the same type as the base.  If the exponent is an :doc:`integer
<integer>`, the result will be :doc:`fractional <fraction>` or
:doc:`rational <rational>`, depending on the type of the base.

Fractional exponents may not be used, as the result could be
irrational.
