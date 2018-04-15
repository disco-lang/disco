
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

The colon in `3 : ℕ` can be read "has type" or "is a", as in "three is
a natural number".  A colon can also be used to give an explicit type
to an expression, for example, when you want to specify a type other
than what disco would infer.  For example:

::

    Disco> :type 3 + 5
    3 + 5 : ℕ
    Disco> :type (3 : Integer) + 5
    (3 : ℤ) + 5 : ℤ

Numeric types
=============

Disco has four primitive numeric types which XXX

* The type of natural numbers, written ``Natural``, ``Nat``, ``N``, or ``ℕ``,
  includes the counting numbers ``0, 1, 2, ...``.
* The type of integers, written ``Integer``, ``Int``, ``Z``, or ``ℤ``,
  includes the natural numbers as well as their negatives.
* The type of positive rationals, written ``QP`` or ``ℚ⁺``, includes
  all ratios of the form :math:`a/b` where :math:`a` and :math:`b` are
  natural numbers with :math:`b \neq 0`.


