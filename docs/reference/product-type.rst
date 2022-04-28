Product types
=============

*Product types*, or *pair types*, represent *pairs* of values. Suppose
``A`` and ``B`` are :doc:`types <types>`. Then:

- ``A * B`` (also written ``A × B``) is a *product type* (also known
  as a *pair type* or *Cartesian product*).  It represents pairs where
  the first element has type ``A`` and the second element has type ``B``.
- If ``a : A`` and ``b : B``, then the ordered pair ``(a, b)`` has
  type ``A * B``.  For example:

    ::

       Disco> :type (1, True)
       (1, true) : ℕ × Bool
       Disco> :type (-7, -3)
       (-7, -3) : ℤ × ℤ

XXX pattern matching

XXX nested tuples
