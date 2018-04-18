
*********
Functions
*********

The type of functions with input ``X`` and output ``Y`` is written ``X
-> Y``.  Some basic examples of function definitions are shown below.

.. literalinclude:: code/function-examples.disco
   :language: idris
   :caption:

* The function ``f`` takes a natural number as input, and returns the
  natural number which is 7 greater.  Notice that ``f`` is defined
  using the syntax ``f(x) = ...``.  In fact, the basic syntax for
  function arguments is juxtaposition, just as in Haskell; the syntax
  ``f x = ...`` would work as well.  Stylistically, however, ``f(x) =
  ...`` is to be preferred, since it matches standard mathematical
  notation.

* The function ``g`` takes an integer ``n`` as input, and returns a
  boolean indicating whether ``n - 3`` is greater than 7.  Note that
  this function cannot be given the type ``N -> Bool``, since it uses
  subtraction.

* The recursive function ``factorial`` computes the factorial of its
  input.  Top-level functions such as ``factorial`` are allowed to be
  recursive.  Notice also that ``factorial`` is defined by two cases,
  which are matched in order from top to bottom, just as in Haskell.

Functions can be given inputs using the same syntax:

::

    Disco> f(2^5)
    39
    Disco> g(-5)
    false
    Disco> factorial(5 + 6)
    39916800

"Multi-argument functions" can be written as functions which take a
product type as input. (This is again a stylistic choice: disco
certainly supports curried functions as well.  But in either case,
disco fundamentally supports only one-argument functions.)  For
example:

.. literalinclude:: code/multi-arg-functions.disco
   :language: idris
   :caption:

All of these examples are in fact *pattern-matching* on their
arguments, although this is most noticeable with the last example.

Anonymous functions
===================

Comparing functions
===================

Let expressions
===============
