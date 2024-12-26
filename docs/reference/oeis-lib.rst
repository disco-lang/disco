oeis
====

Disco provides a library for interfacing with the `Online Encyclopedia
of Integer Sequences <OEIS_>`_ (OEIS).  You can :doc:`import
<import>` it with:

.. _OEIS: https://oeis.org/

::

   import oeis

The library provides two functions:

* The ``lookupSequence`` function takes a list of natural numbers and
  returns the URL of the first result in the OEIS.  For example:

  ::

     Disco> lookupSequence [1,1,2,3,5,8]
     right("https://oeis.org/A000045")

  In this example, the returned URL is in fact the OEIS page for the
  `Fibonacci numbers <fib_>`_.

* The ``extendSequence`` function tries to *extend* the given list as
  far as it can, using data from the first match on the OEIS.  Using
  the same example as before, just by putting in the first few
  Fibonacci numbers we can get a lot more:

  ::

     Disco> extendSequence [1,1,2,3,5,8]
     [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227465, 14930352, 24157817, 39088169, 63245986, 102334155]

.. _fib: https://oeis.org/A000045
