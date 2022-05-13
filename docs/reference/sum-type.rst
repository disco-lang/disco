Sum types
=========

*Sum types* represent situations where we have a value which could be
*either one thing or another*.  Suppose ``A`` and ``B`` are :doc:`types <types>`. Then:

- ``A + B`` is a *sum type* (also known as a *disjoint union*).  It
  represents a *disjoint union* of the types ``A`` and ``B``.  That
  is, the values of ``A + B`` can be either a value of type ``A``, or
  a value of type ``B``.

- A value of type ``A + B`` can be written either ``left(a)``, where
  ``a`` is an arbitrary :doc:`expression <expression>` of type ``A``,
  or ``right(b)``, where ``b`` is an arbitrary expression of type
  ``B``.  For example:

    ::

       Disco> left(3) : N + Bool
       left(3)
       Disco> right(false) : N + Bool
       right(false)

Note that the ``left`` or ``right`` ensures that ``A + B`` really does
represent a *disjoint* union.  For example, although the usual
:doc:`union <union>` operator is idempotent, that is,
:math:`\mathbb{N} \cup \mathbb{N} = \mathbb{N}`, with a disjoint union
of types ``N + N`` is not at all the same as ``N``.  Elements of ``N +
N`` look like either ``left(3)`` or ``right(3)``, that is, ``N + N``
includes *two* copies of each natural number.
