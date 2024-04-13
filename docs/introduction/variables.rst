Variables
=========

We can create *variables* in Disco as names to stand for a certain
value.  Variable names can be any combination of lower and uppercase
letters, digits, underscores (``_``), and single quotes (``'``), but
they must start with a letter.  For example, these are all valid
variable names:

::

   f
   FROG
   f_99x'
   e1E10

On the other hand, these are not valid as variable names:

::

   _x
   2nd_try
   m&m's

To create a variable, there are two steps.

1. We must first *declare* the variable's :doc:`type <types>`,
   that is, say what type of value it will hold. We do this by writing
   the variable's name, a colon, and a type.  For example:

   ::

      x : N

2. We can then *define* the variable to be equal to the value of some
   *expression*.  For now, just think of an expression as any combination of
   numbers, operations, parentheses, and other variables.  For
   example:

   ::

      x = (3 + 77) * 5

Expressions are *evaluated* to find out what value they have, using
the same order of operations you are used to from math.  For example,
to evaluate ``3 + 7 * 5`` Disco will first multiply 7 by 5, then add 3.

Exercises
---------
