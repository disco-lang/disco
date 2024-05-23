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

Order of operations and operator precedence
-------------------------------------------

You probably remember learning about the standard "order of
operations"; Disco knows about this too.  For example, ``1 +
2 * 3`` evaluates to ``7``, not ``9``, since Disco knows the
multiplication should be done first:

::

   Disco> 1 + 2 * 3
   7
   Disco> 1 + (2 * 3)  -- this is the same as 1 + 2 * 3
   7
   Disco> (1 + 2) * 3  -- this is different
   9

The "order of operations" really has to do with *where parentheses
should go*.  If we always wrote parentheses around absolutely
everything, then we wouldn't need an order of operations at all.  For
example,

::

   Disco> x : N
   Disco> x = 2
   Disco> (((2^2) + (3 * x)) + 1) + ((2 + 5)*(x^3))
   67

But writing things this way would be incredibly tedious. Agreeing on
an order of operations---that is, which operations have higher
*precedence*, or priority, than others---allows us to leave out
some parentheses.

::

   Disco> 2^2 + 3*x + 1 + (2 + 5) * x^3
   67

Of course, Disco has many more operators than we have seen so far
(almost 30 in total).  But you do not need to memorize the precedence
(*i.e.* order of operations) for all the operators!  You can use the
``:doc`` command to see information about an operator, including its
precedence level.  For example, let's check out the documentation for
``+``:

::

   Disco> :doc +
   ~+~ : ℕ × ℕ → ℕ
   precedence level 7, left associative

   The sum of two numbers, types, or graphs.

   https://disco-lang.readthedocs.io/en/latest/reference/addition.html

There is a lot of information here, so let's go through it slowly.
The first line, ``~+~ : ℕ × ℕ → ℕ``, tells us the :doc:`type <types>`
of the addition operator.  Don't worry for now about what the various
symbols like ``~``, ``×``, and ``→`` mean; essentially this is telling
us that ``+`` takes a pair of natural numbers and returns a natural
number (XXX why this type is wrong --- ``+`` is more general).

The second line tells us the *precedence level* of ``+`` is ``7``.
Operators with a *higher* precedence level come before ones with a
lower precedence (for example, ``*`` has precedence level ``8``).  It
also tells us that ``+`` is *left associative*, which means that if we
use multiple ``+`` operations in a row (like ``1 + 2 + 3 + 4``), they
will be done from left to right (like ``((1 + 2) + 3) + 4``).

Finally, there is a description of what the operator does, and a link
we can click if we want to read more about it.

Exercises
---------

Rewrite each of the following expressions in an equivalent way using
as few parentheses as possible.  Use the :doc command if you need to
know the precedence of an operator.  Use Disco to make sure that the
original expression and your new version still yield the same result.

* ``XXX``

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
