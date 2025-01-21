Arithmetic
==========

You have already seen some basic arithmetic, in the :doc:`getting
started <getting-started>` section.  In this section, we'll dive
deeper into the arithmetic operations supported by Disco.

Addition and multiplication
---------------------------

The most basic arithmetic operations Disco supports are :doc:`addition
</reference/addition>` and :doc:`multiplication
</reference/multiplication>`.

- Addition is written using ``+``, for example, ``3 + 5 + 2/3``.
- Multiplication is written using ``*``, for example, ``(-2) * 6 *
  (1/3)``.
- The multiplication sign can also :doc:`sometimes
  </reference/multiplication>` be omitted, just like in standard
  mathematical notation.  For example:

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

All the numeric types (:doc:`natural numbers </reference/natural>`
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
(almost 30 in total), but you do not need to memorize the precedence
(*i.e.* order of operations) for all the operators!  You can use the
``:doc`` command to see information about an operator, including its
precedence level.  For example, let's check out the
:doc:`documentation </reference/docs>` for ``+``:

::

   Disco> :doc +
   This expression has multiple possible types.  Some examples:
   ~+~ : ℕ × ℕ → ℕ
   ~+~ : ℤ × ℤ → ℤ
   ~+~ : 𝔽 × 𝔽 → 𝔽
   ~+~ : ℚ × ℚ → ℚ
   precedence level 7, left associative

   The sum of two numbers, types, or graphs.

   https://disco-lang.readthedocs.io/en/latest/reference/addition.html

There is a lot of information here, so let's go through it slowly.
The first few lines tell us some of the :doc:`types`
the addition operator can have.  Don't worry for now about what the various
symbols like ``~``, ``×``, and ``→`` mean; essentially this is telling
us that ``+`` takes a pair of natural numbers and returns a natural
number; or a pair of integers and returns an integer; and so on.  This
fits with the claim made before that all four of the numeric types
support addition.

The next line tells us that the *precedence level* of ``+`` is ``7``.
It also tells us that ``+`` is *left associative*, which means
that if we use multiple ``+`` operations in a row (like ``1 + 2 + 3 +
4``), they will be done from left to right (like ``((1 + 2) + 3) +
4``).

Finally, there is a description of what the operator does, and a link
we can click if we want to read more about it.

If we look at the documentation for multiplication, we can see that it
has a *higher* precedence (8) than addition:

::

   Disco> :doc *
   ~*~ : ℕ × ℕ → ℕ
   precedence level 8, left associative

   The product of two numbers, types, or graphs.

   https://disco-lang.readthedocs.io/en/latest/reference/multiplication.html

The higher precedence level of ``*`` is how Disco knows that it should
come before (*i.e.* have parentheses put around it before) addition.

Precedence exercises
--------------------

* What is the precedence level of subtraction, and how does it compare
  to the precedence levels of addition and multiplication?  Does this
  make sense given what you know about the order of operations?

* What is the precedence level of the "less than" operator ``<``?
  Does it have higher or lower precedence than addition?  Does this
  make sense?  (*Hint*: think about expressions such as ``y < x + 3``.)

* Rewrite each of the following expressions in an equivalent way using
  as few parentheses as possible.  Use the ``:doc`` command if you
  need to look up the precedence of an operator.  Use Disco to make
  sure that the original expression and your new version still yield
  the same result.

    * ``((1 + 2) + 3) + 4``
    * ``(1 + 2) + (3 + 4)``
    * ``1 + (5 * (x^2))``
    * ``((((2 + 3) * 5) + 2) * 10) * 2``
    * ``x^(2^(3^1))``

Subtraction and absolute value
------------------------------

We can also perform subtraction in Disco, using the usual ``-``
operator.  As mentioned before, we can only do subtraction on integers
(``Z``) and rational numbers (``Q``); however, remember that other
numeric types can be automatically converted into one of these.

The absolute value function is written ``|x|`` or ``abs(x)``.  It's
worth noting that absolte value turns integers into natural numbers,
and rational numbers into fractional numbers.  For example:

::

   Disco> :type -3
   -3 : ℤ
   Disco> :type |-3|
   abs(-3) : ℕ

Division
--------

Division can be performed in Disco, using the ``/`` operator.  As you
learned in the section on :doc:`types`, only fractional numbers
(``F``) and rational numbers (``Q``) support division; however,
natural numbers or integers can be converted to those types as
necessary.

::

   Disco> :type 3
   3 : ℕ
   Disco> :type (-5)
   -5 : ℤ
   Disco> :type 3/(-5)
   3 / (-5) : ℚ
   Disco> 3/(-5)
   -3/5

Division in Disco always gives an exact answer; it never rounds down
or gives an approximate result.

Floor and ceiling
-----------------

In many cases, we might want to round some number to an integer.
Disco provides the ``floor`` and ``ceiling`` functions for this
purpose.

* ``floor(x)`` rounds ``x`` *down* to the nearest integer.  In other
  words, ``floor(x)`` is the largest integer which is less than or
  equal to ``x``.  As an alternative, Disco also supports the standard
  mathematical notation ``⌊x⌋`` instead of ``floor(x)``.
* Likewise, ``ceiling(x)`` rounds *up* to the nearest integer, that
  is, it results in the smallest integer greater than or equal to
  ``x``.  As an alternative, Disco also supports the standard
  mathematical notation ``⌈x⌉`` instead of ``ceiling(x)``.
* Disco does not provide a built-in ``round`` function for rounding to
  the *nearest* integer; however, you can use ``floor(x + 1/2)`` for
  this purpose.

Note that ``floor`` and ``ceiling`` turn rational numbers into
integers, and fractional numbers into natural numbers.  In other
words, they have types like:

::

   floor : 𝔽 → ℕ
   floor : ℚ → ℤ

Integer division
----------------

One common application for ``floor`` is *integer division*, that is,
dividing two integers and rounding the result down to the nearest
integer.  Integer division can therefore be written ``floor(x / y)``.
However, this is such a common operation that Disco provides a
built-in integer division operator ``//``, so ``x // y`` is shorthand
for ``floor(x / y)``.

Exponentiation
--------------

Exponentiation in Disco can be written using the ``^`` operator, just
like most calculators.  For example:

::

   Disco> 2 ^ 4
   16
   Disco> 5 ^ 1
   5
   Disco> 3 ^ 0
   1
   Disco> 2 ^ (-3)
   1/8
   Disco> 2 ^ 500
   3273390607896141870013189696827599152216642046043064789483291368096133796404674554883270092325904157150886684127560071009217256545885393053328527589376

Note that exponentiation can handle exponents which are zero or negative.

The type of exponentiation is somewhat complex, but it is not
too important to understand at the moment.

.. admonition:: To be written

   * Exercises
