Arithmetic
==========

You have already seen some basic arithmetic, in the :doc:`getting
started <getting-started>` section.  In this section, we'll dive
deeper into the arithmetic operations supported by Disco.

Addition and multiplication
---------------------------

The most basic arithmetic operations Disco supports are addition and
multiplication.

- Addition is written using ``+``, for example, ``3 + 5 + 2/3``.
- Multiplication is written using ``*``, for example, ``(-2) * 6 *
  (1/3)``.
- The multiplication sign can also sometimes be omitted, just like in
  standard mathematical notation.  For example:

  ::

     Disco> x : N
     Disco> x = 9
     Disco> 2 * x
     18
     Disco> 2x   -- means the same as 2*x
     18
     Disco> (1 + 3) * (5 - 2)
     12
     Disco> (1 + 3)(5 - 2)  -- means the same as above
     12

All the number types (:doc:`natural numbers </reference/natural>`
``N``, :doc:`integers </reference/integer>` ``Z``, :doc:`fractional
numbers </reference/fraction>` ``F``, and :doc:`rational numbers
</reference/rational>` ``Q``) support both addition and
multiplication.

Order of operations
-------------------

Parentheses. Multiplication and division left to right.  Addition and
subtraction left to right.

Subtraction
-----------

Subtraction is written using ``-``.  Also ``.-`` for saturating subtraction.

Only Z, Q support subtraction.

Absolute value
--------------

Written ``|x|`` or ``abs(x)``.

Types: turns Z into N, Q into F.  Show examples.

Division
--------

Written using ``/``.  Only F, Q support division.  Show examples.

Integer division
----------------

Division on N, Z that rounds down.

Floor and ceiling
-----------------

``floor(x)``, ``ceiling(x)``.  Definitions.  Cool Unicode notation.
Turns Q into Z, F into N.

Note that ``x // y`` is really just shorthand for ``floor(x / y)``.

Exponentiation
--------------
