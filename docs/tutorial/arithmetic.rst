
**********
Arithmetic
**********

As a computational platform for learning discrete mathematics, at the
core of disco is of course the ability to compute with numbers.
However, unlike many other languages, disco does not support
real (*aka* floating-point) numbers at all---they are not typically
needed for discrete mathematics, and omitting them simplifies the
language quite a bit.   To compensate, however, disco has
sophisticated built-in support for rational numbers.

Basic arithmetic
================

To start out, you can use disco as a simple calculator.  For
example, try entering the following expressions, or others like them,
at the ``Disco>`` prompt:

* ``2 + 5``
* ``5 - 8``
* ``5 * (-2)``
* ``(1 + 2)(3 + 4)``
* ``2 ^ 5``
* ``2 ^ 5000``
* ``4 .- 2``
* ``2 .- 4``

The last two expressions use the saturating subtraction operator, ``.-``, which
takes two numeric operands, :math:`a` and :math:`b`, and returns :math:`a - b`
if :math:`a > b`, and :math:`0` otherwise. Note that unlike regular subtraction,
the result of a saturating subtraction will always be a natural number.

Also notice that it is not always necessary to write ``*`` for
multiplication: as is standard mathematical notation, we may often
omit it, as in ``(1 + 2)(3 + 4)``, which means the same as ``(1 + 2) *
(3 + 4)``. (For precise details on when the asterisk may be omitted,
see the discussion in the section on functions.)  Notice also that
integers in disco may be arbitrarily large.

Now try these:

* ``3/7 + 2/5``
* ``2 ^ (-5)``

The results may come as a bit of a surprise if you are already used to
other languages such as Java or Python, which would yield a
floating-point (*i.e.* real) number; as mentioned before, disco does
not support floating-point numbers. However, rational numbers can
still be entered using decimal notation.  Try these expressions as
well:

* ``2.3 + 1.6``
* ``1/5.``
* ``1/7.``

Disco automatically picks either fractional or decimal notation for
the output, depending on whether any values with decimal points were
used in the input (for example, compare ``1/5`` and ``1/5.``, or
``1./5``).  Note that ``1/7.`` results in ``0.[142857]``;
can you figure out what the brackets indicate?

The standard ``floor`` and ``ceiling`` operations are built-in:

::

    Disco> floor (17/3)
    5
    Disco> ceiling (17/3)
    6

Just for fun, disco also supports standard mathematical notation for
these operations via Unicode characters:

::

    Disco> ⌊ 17/3 ⌋
    5
    Disco> ⌈ 17/3 ⌉
    6

Integer division, which rounds down to the nearest integer, can be
expressed using ``//``:

::

    Disco> 5 // 2
    2
    Disco> (-5) // 2
    -3

``x // y`` is always equivalent to ``floor (x/y)``, but is provided as
a separate operator for convenience.

The counterpart to integer division is ``mod``, which gives the
remainder when the first number is divided by the second:

::

    Disco> 5 mod 2
    2
    Disco> (2^32) mod 7
    4
    Disco> (2^32) % 7

The ``%`` operator may also be used as a synonym for ``mod``.

Finally, the ``abs`` function is provided for computing absolute
value:

::

    Disco> abs 5
    5
    Disco> abs (-5)
    5

Advanced arithmetic
===================

Disco also provides a few more advanced arithmetic operators which you
might not find built in to other languages.

* The ``divides`` operator can be used to test whether one number
  evenly divides another.  Try evaluating these expressions:

    * ``2 divides 20``
    * ``2 divides 21``
    * ``(-2) divides 20``
    * ``2 divides (-20)``
    * ``7 divides (2^32 - 4)``
    * ``(1/2) divides (3/2)``
    * ``(1/5) divides (3/2)``
    * ``1 divides 10``
    * ``0 divides 10``
    * ``10 divides 0``
    * ``0 divides 0``

    The last three expressions may be surprising, but follow directly
    from the definition: ``a divides b`` is true if there is an
    integer ``k`` such that ``a*k = b``.  For example, there is no
    ``k`` such that ``0*k = 10``, so ``0 divides 10`` is false.

    Note that a vertical line is often used to denote divisibility, as
    in :math:`3 \mid 21`, but disco does not support this notation, since
    the vertical line is used for other things (and besides, it is
    typically not a good idea to use a visually symmetric operator for
    a nonsymmetric relation).

* The ``choose`` operator can be used to compute binomial
  coefficients.  For example, ``5 choose 2`` is the number of ways to
  select two things out of five.

* The factorial function is available via standard mathematical
  notation:

    ::

        Disco> 20!
        2432902008176640000

* Square root (``sqrt``) and base-two logarithm (``lg``) functions are
  provided which round their result down to the nearest integer
  (remember that disco does not support arbitrary real numbers).

    ::

        Disco> sqrt (299^2 + 1)
        299
        Disco> sqrt (299^2 .- 1)
        298
        Disco> lg (2^35 + 7)
        35
        Disco> lg (2^35 .- 1)
        34
