Pair types
==========

*Pair types*, or *product types*, represent *ordered pairs* of
values. Suppose ``A`` and ``B`` are :doc:`types <types>`. Then:

- ``A * B`` (also written ``A × B``) is a *pair type* (also known
  as a *product type* or *Cartesian product*).  It represents the set of
  all possible pairs where the first element has type ``A`` and the
  second element has type ``B``.

- A pair is written ``(a, b)`` (where ``a`` and ``b`` can be arbitrary
  :doc:`expressions <expression>`.  Specifically, if ``a : A`` and ``b : B``, then the
  ordered pair ``(a, b)`` has type ``A * B``.  For example:

    ::

       Disco> :type (1, True)
       (1, true) : ℕ × Bool
       Disco> :type (-7, -3)
       (-7, -3) : ℤ × ℤ

Pair types are commonly used to represent functions that take multiple
inputs.  For example, ``f : N * Z -> Q`` means that ``f`` takes a
*pair* of a natural number and an integer as input.  Such functions
are often defined via :doc:`pattern matching <pattern>` on the pair,
:doc:`like so <prod-pattern>`:

::

   f : N * Z -> Z
   f(n,z) = 3n - z

Nested pairs and n-tuples
=========================

Coming soon!
