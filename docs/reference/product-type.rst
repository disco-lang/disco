Pair types
==========

*Pair types*, or *product types*, represent *ordered pairs* of
values. Suppose ``A`` and ``B`` are :doc:`types <types>`. Then:

- ``A * B`` (also written ``A × B``) is a *pair type* (also known
  as a *product type* or *Cartesian product*).  It represents the set of
  all possible pairs where the first element has type ``A`` and the
  second element has type ``B``.

- A pair is written ``(a, b)`` (where ``a`` and ``b`` can be arbitrary
  :doc:`expressions <expression>`).  Specifically, if ``a : A`` and ``b : B``, then the
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


n-tuples and nested pairs
=========================

We have seen that ``A * B`` is a type of *pairs* of values.  What
about triples, quadruples, ... n-tuples of values?  The answer is
simple:

- triples are written ``(x,y,z)`` and have types like ``A * B * C``;
- quadruples are written ``(w,x,y,z)`` and have types like ``A * B *
  C * D``;
- and so on.

So, for example, a function taking a quintuple of values could be
written like this:

::

   funTaking5Tuple : N * Z * List(N) * Q * Bool -> Int
   funTaking5Tuple(n,z,l,q,b) = ...

.. note::

   General n-tuples actually are not specially built in at all:
   rather, everything is actually built out of *nested pairs*.  For
   convenience, pair types and values both *associate to the right*,
   that is,

   - the type ``A * B * C`` is interpreted as ``A * (B * C)``, and
   - correspondingly, ``(x, y, z)`` is interpreted as ``(x, (y, z))``.

   So, for example, the definition of the function ``funTaking5Tuple``
   from above is really shorthand for

   ::

      funTaking5Tuple : N * (Z * (List(N) * (Q * Bool))) -> Int
      funTaking5Tuple(n,(z,(l,(q,b)))) = ...

   Typically one can just use triples or 5-tuples or whatever and not
   think about this, but occasionally it's helpful to understand the
   way things are represented with nested pairs under the hood.
