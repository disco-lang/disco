
*****
Types
*****

Every value in disco has a *type*.  Types play a central role in the
language, and help guide and constrain programs.  All the types in a
program must match correctly (*i.e.* the program must *typecheck*)
before it can be run.  The type system has for the most part been
designed to correspond to common mathematical practice, so if you are
used to type systems in other programming languages (even other
functional languages) you may be in for a surprise or two.

Disco can often infer the type of an expression.  To find out what
type disco has inferred for a given expression, you can use the
``:type`` command.  For example:

::

    Disco> :type 3
    3 : ℕ
    Disco> :type 2/3
    2 / 3 : 𝔽
    Disco> :type [1,2,5]
    [1, 2, 5] : List ℕ

The colon in ``3 : ℕ`` can be read "has type" or "is a", as in "three is
a natural number".  A colon can also be used to give an explicit type
to an expression, for example, when you want to specify a type other
than what disco would infer.  For example:

::

    Disco> :type 3 + 5
    3 + 5 : ℕ
    Disco> :type (3 : Integer) + 5
    (3 : ℤ) + 5 : ℤ

The above example shows that normally, disco infers the type of ``3 +
5`` to be a natural number, but we can force the ``3`` to be treated as
an ``Integer``, which in turn forces the whole expression to be inferred
as an integer.

Primitive numeric types
=======================

Disco has four built-in primitive numeric types: natural numbers,
integers, fractions (*i.e.* nonnegative rationals), and rationals.

* The type of natural numbers, written ``Natural``, ``Nat``, ``N``, or ``ℕ``,
  includes the counting numbers :math:`0, 1, 2, \dots`.
* The type of integers, written ``Integer``, ``Int``, ``Z``, or ``ℤ``,
  includes the natural numbers as well as their negatives.
* The type of fractions (*i.e.* nonnegative rationals), written
  ``Fractional``, ``Frac``, ``F``, or ``𝔽``, includes all ratios of
  the form :math:`a/b` where :math:`a` and :math:`b` are natural
  numbers, with :math:`b \neq 0`.
* The type of rational numbers, written ``Rational``, ``Q`` or ``ℚ``,
  includes all ratios of integers.

In mathematics, it is typically not so common to think of the
nonnegative rationals :math:`\mathbb{F}` as a separate set by
themselves; but this is mostly for historical reasons and because of
the way the development of rational numbers is usually presented.  The
natural numbers support addition and multiplication.  Extending them
to support subtraction yields the integers; then, extending these
again to support division yields the rationals.  However, what if we
do these extensions in the opposite order?  Extending the natural
numbers to support division results in the positive rational numbers;
then extending these with subtraction again yields the rationals.  All
told, the relationship between these four types forms a diamond-shaped
lattice:

::

      Q
     / \
    Z   F
     \ /
      N


Each type is a subset of the type or types above it.  Going northwest
in this diagram (:math:`\mathbb{N} \to \mathbb{Z}` or
:math:`\mathbb{F} \to \mathbb{Q}`) corresponds to allowing negatives,
that is, subtraction; going northeast (:math:`\mathbb{N} \to
\mathbb{F}` or :math:`\mathbb{Z} \to \mathbb{Q}`) corresponds to
allowing reciprocals, that is, division.

Try evaluating each of the following expressions at the disco prompt,
and also request their inferred type with the ``:type`` command.  What
type does disco infer for each? Why?

* ``1 + 2``
* ``3 * 7``
* ``1 - 2``
* ``1 / 2``
* ``(1 - 2) / 3``

Going southeast in the lattice (getting rid of negatives) is
accomplished with the absolute value function ``abs``.  Going
southwest (getting rid of fractions) is accomplished with ``floor``
and ``ceiling``.

Note that disco supports *subtyping*, that is, values of type ``S`` can
be automatically "upgraded" to another type ``T`` as long as ``S`` is
a "subtype" (think: subset) of ``T``.  For example, a natural number
can be automatically upgraded to an integer.

::

    Disco> (-1 : Z) + (3 : N)
    2
    Disco> :type (-1 : Z) + (3: N)
    (-1 : ℤ) + (3 : ℕ) : ℤ

In the above example, the natural number ``3`` is automatically
upgraded to an integer so that it can be added to ``-1``.  When we
discuss functions later, we will see that this principle extends to
function arguments as well: for example, if a function is expecting an
integer as input, it is acceptable to give it a natural number, since
the natural number can be upgraded to an integer.

Other types
===========

There are many other types built into disco as well---``Bool``, ``Void``,
``Unit``, ``List``, product, and sum types, to name a few.  These will be
covered throughout the rest of the tutorial in appropriate places.
For now, try executing these commands and see if you can guess what is
going on:

* ``:type false``
* ``:type ()``
* ``:type [1, 2, 3]``
* ``:type [1, 2, -3]``
* ``:type [1, 2, -3, 4/5]``
* ``:type [[1,2], [3,4,5]]``
* ``:type (1, true)``
* ``:type left(3)``
