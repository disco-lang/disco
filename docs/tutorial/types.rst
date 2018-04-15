
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

Numeric types
=============

Disco has four primitive numeric types which XXX

The type of natural numbers, written ``Nat``, ``N``, or ``ℕ``,
includes the numbers ``0, 1, 2, ...``


