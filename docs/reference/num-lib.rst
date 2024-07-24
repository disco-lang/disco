num
===

The Disco standard number theory library can be :doc:`imported
<import>` with

::

   import num

It provides the following functions:

* ``isPrime : N -> Bool`` tests whether a given natural number is
  prime.  It would be possible to implement this function in Disco,
  but this built-in version is much more efficient.  For example it
  works on 15-digit numbers almost instantly.

* ``log(b,n)`` calculates the logarithm base ``b`` of ``n``, rounded
  down to the nearest natural number.

* For convenience, ``lg`` is defined as the base-2 logarithm
  specifically.  ``lg(n) + 1`` is the number of bits needed to
  represent ``n`` in binary.

* ``factor`` takes a natural number and returns a :doc:`bag <bag>` of its
  prime factors.  Like ``isPrime``, this function could in theory be
  written in Disco itself, but the provided built-in version is
  extremely efficient.

* ``even`` and ``odd`` test whether a given integer is even or odd,
  respectively.
