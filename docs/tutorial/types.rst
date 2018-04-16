
*****
Types
*****

Every value in disco has a *type*.  Types play a central role in the
language and help XXX.  However, the type system has for the most part
been designed to correspond to common mathematical practice, so XXX.

Disco can often infer the type of an expression.  To find out what
type disco has inferred for a given expression, you can use the
``:type`` command.  For example:

::

    Disco> :type 3
    3 : ℕ
    Disco> :type 2/3
    2 / 3 : ℚ⁺
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
integers, positive rationals, and rationals.

* The type of natural numbers, written ``Natural``, ``Nat``, ``N``, or ``ℕ``,
  includes the counting numbers :math:`0, 1, 2, \dots`.
* The type of integers, written ``Integer``, ``Int``, ``Z``, or ``ℤ``,
  includes the natural numbers as well as their negatives.
* The type of positive rationals, written ``QP`` or ``ℚ⁺``, includes
  all ratios of the form :math:`a/b` where :math:`a` and :math:`b` are
  natural numbers, with :math:`b \neq 0`.
* The type of rational numbers, written ``Rational``, ``Q`` or ``ℚ``,
  includes all ratios of integers.

In mathematics, it is typically not so common to think of the positive
rationals :math:`\mathbb{Q}^+` as a separate set by themselves; but
this is mostly for historical reasons and because of the way the
development of rational numbers is usually presented.  The natural
numbers support addition and multiplication.  Extending them to
support subtraction yields the integers; then, extending these again
to support division yields the rationals.  However, what if we do
these extensions in the opposite order?  Extending the natural numbers
to support division results in the positive rational numbers; then
extending these with subtraction again yields the rationals.  All
told, the relationship between these four types forms a diamond shape:

::

      Q
     / \
    Z   Q+
     \ /
      N

Each type is a subset of the type or types above it.  Going northwest
in this diagram (:math:`\mathbb{N} \to \mathbb{Z}` or
:math:`\mathbb{Q}^+ \to \mathbb{Q}`) corresponds to adding negatives,
that is, subtraction; going northeast (:math:`\mathbb{N} \to
\mathbb{Q}^+` or :math:`\mathbb{Z} \to \mathbb{Q}`) corresponds to
adding reciprocals, that is, division.

XXX examples.  Show subtyping.  Show how types of different operations
works.

Finite types
============

XXX to do
