Arithmetic
==========

You have already seen some basic arithmetic, in the :doc:`getting
started <getting-started>` section.  In this section, we'll dive
deeper into the arithmetic operations supported by Disco.

Addition and multiplication
---------------------------

Addition is written using ``+``.

Multiplication is written using ``*``, or sometimes it can be omitted.

All the number types (N, Z, F, Q) support both addition and multiplication.

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
