
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
arguments, although this is most noticeable with the last example,
which decomposes its input into a pair of pairs and gives a name to
each component.

Anonymous functions
===================

Comparing functions
===================

In certain cases, functions can be compared for equality, or even
compared to see which is less or greater.

::

    Disco> ((x:Bool) -> x) = ((x:Bool) -> not (not x))
    true
    Disco> ((x:Bool) -> x) = ((x:Bool) -> not x)
    false

There is no magic involved, and it does not work by looking at the
definitions of the functions. Simply put, two functions are equal if
they give the same output for every input.  So disco can only test two
functions for equality if they have a finite input type, in which case
it simply enumerates all possible values of the input type, and tests
that the two functions give equal outputs for every input.

Functions are ordered by conceptually listing all their outputs
ordered by inputs (that is, list the values of the input type in order
from smallest to largest and apply the function to each) and then
comparing these lists of outputs lexicographically.  That is, if ``i``
is the smallest possible input value and ``f i < g i``, then ``f <
g``.  If ``f i = g i``, then we move on to consider the second
smallest input value, and so on.

Let expressions
===============
