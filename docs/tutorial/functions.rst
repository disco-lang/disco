
*********
Functions
*********

The type of functions with input ``X`` and output ``Y`` is written ``X
-> Y``.  Some basic examples of function definitions are shown below.

.. literalinclude:: example/function.disco
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

.. literalinclude:: example/multi-arg-functions.disco
   :language: idris
   :caption:

All of these examples are in fact *pattern-matching* on their
arguments, although this is most noticeable with the last example,
which decomposes its input into a pair of pairs and gives a name to
each component.

Functions in disco are first-class, and can be provided as input to
another function or output from a function, stored in data structures,
*etc*.  For example, here is how one could write a higher-order
function to take a function on natural numbers and produce a new
function which iterates the original function three times:

.. literalinclude:: example/higher-order.disco
   :language: idris
   :caption:

Anonymous functions
===================

The syntax for an anonymous function in disco consists of three parts:
one or more *bindings*, followed by a *mapsto* symbol, followed by an arbitrary
disco expression.

* Each *binding* specifies the name of an input to the function.  A binding
  can be either a simple variable name, or a parenthesized variable
  name with a type annotation (*e.g.* ``(x:Nat)``).  There can be
  multiple bindings separated by whitespace, which creates a (curried)
  "multi-argument" function.
* disco will accept any one of several syntaxes for the *mapsto*
  symbol: either ``->``, ``|->``, or ``↦``.

.. note::

   It's quite possible this syntax might change.  For example, we
   might want to disallow ``->`` as a mapsto symbol, since that may
   cause confusion with the same symbol used as part of a type.  Also, we
   might want to require "lambda" syntax before the binding (*e.g.*
   either a backslash or an actual lambda).

   The current syntax was designed to mirror the syntax in most common
   mathematical practice (*e.g.* :math:`x \mapsto x^2 + 3`), but it's
   quite possible discrete math students will not be familiar with
   that notation anyway, in which case we might as well introduce them
   to the lambda calculus.

   Currently, bindings cannot contain patterns, but in general we
   might want to allow this, for example, ``((x,y) |-> x + y) : N*N -> N``.

Here are a few examples of using anonymous functions as arguments
to ``thrice``:

::

    Disco> thrice(x |-> x*2)(1)
    8
    Disco> thrice((z:Nat) ↦ z^2 + 2z + 1)(7)
    17859076

TODO example of using multi-argument anonymous function

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

Disambiguating function application and multiplication
======================================================

As previously mentioned, the fundamental syntax for applying a
function to an argument is *juxtaposition*, that is, simply putting
the function next to its argument (with a space in between if
necessary).

However, disco also allows multiplication to be written in this way.
How can it tell the difference? Given an expression of the form ``X
Y`` (where ``X`` and ``Y`` may themselves be complex expressions),
disco uses simple *syntactic* rules to distinguish between
multiplication and function application.  In particular, note that the
*types* of ``X`` and ``Y`` do not enter into it at all (it would
greatly complicate matters if parsing and typechecking had to be
interleaved---even though this is what human mathematicians do in
their heads; see the discussion below).

To decide whether ``X Y`` is function application or multiplication,
disco looks only at the syntax of ``X``; ``X Y`` is multiplication if
and only if ``X`` is a *multiplicative term*, and function application
otherwise.  A multiplicative term is one that looks like either a
natural number literal, or a unary or binary operation (possibly in
parentheses).  For example, ``3``, ``(-2)``, and ``(x + 5)`` are all
multiplicative terms, so ``3x``, ``(-2)x``, and ``(x + 5)x`` all get parsed as
multiplication.  On the other hand, an expression like ``(x y)`` is always parsed as
function application, even if x and y both turn out to have numeric types
types; a bare variable like x does not count as a multiplicative term.
Likewise, ``(x y) z`` is parsed as function application, since ``(x y)`` is
not a multiplicative term.

.. note::

   You may enjoy reflecting on how a *human* mathematician does this
   disambiguation.  In fact, they are doing something much more
   sophisticated than disco, implicitly using information about types
   and social conventions regarding variable names in addition to
   syntactic cues.  For example, consider :math:`x(y + 3)` versus :math:`f(y +
   3)`. Most mathematicians would unconsciously interpret the first
   as multiplication and the second as function application, due to
   standard conventions about the use of variable names :math:`x` and
   :math:`f`.  On the other hand, in the sentence "Let :math:`x` be the function
   which doubles an integer, and consider :math:`v = x(y+3)`", any
   mathematician would have no trouble identifying this use of
   :math:`x(y+3)` as function application, although they might also
   rightly complain that :math:`x` is a strange choice for the name of
   a function.

Let expressions
===============

TODO

* Optional type annotations
* Multiple bindings separated by commas
* Each binding scopes over later bindings
* No recursion
