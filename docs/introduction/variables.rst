Variables
=========

We can create *variables* in Disco as names to stand for certain
values.  Variable names can be any combination of lower and uppercase
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

1. We must first *declare* the variable's :doc:`type <types>`, that
   is, we must say what type of value the variable will hold. We do
   this by writing the variable's name, a colon, and a type.  For
   example:

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

Once we have created a variable, we can use it anywhere, and it will
be replaced by its value.  For example:

::

   Disco> x : N
   Disco> x = (3 + 77) * 5
   Disco> x
   400
   Disco> x + 7
   407
   Disco> x * x - 99
   159901

Exercises
---------

* Define a natural number variable named ``vol`` and give it a value
  equal to the volume of a 6 by 7 by 8 box.  Use it to evaluate the
  following expressions:

    - ``vol + vol``
    - ``3 vol^2 + 2 vol - 7``
    - ``(vol - 8)(vol + 8)``
