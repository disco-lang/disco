
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

The syntax for an anonymous function in disco consists of a *lambda*
(either a backslash or an actual ``λ``) followed by a pattern, a
period, and an arbitrary disco expression (the *body*).

The pattern can be a single variable name or a more complex
pattern. Note that patterns can also contain type annotations.  Unlike
in, say, Haskell, there is no special syntactic sugar for curried
multi-argument functions; one can just write nested lambdas.

Here are a few examples to illustrate the possibilities:

::

    Disco> thrice(\x. x*2)(1)
    8
    Disco> thrice(\z:Nat. z^2 + 2z + 1)(7)
    17859076
    Disco> (\(x,y). x + y) (3,2)
    5
    Disco> (\x:N. \y:Q. x > y) 5 (9/2)
    true

Let expressions
===============

*Let expressions* are a mechanism for defining new variables for local
use within an expression.  For example, ``3 + (let y = 2 in y + y)``
evaluates to ``7``: the expression ``y + y`` is evaluated in a context
where ``y`` is defined to be ``2``, and the result is then added to
``3``.  The simplest syntax for a let expression, as in this example,
is ``let <variable> = <expression1> in <expression2>``.  The value of
the let expression is the value of ``<expression2>``, which may
contain occurrences of the ``<variable>``; any such occurrences will
take on the value of ``<expression1>``.

More generally:

* A ``let`` may have multiple variables defined before ``in``,
  separated by commas.
* Each variable may optionally have a type annotation.
* The definitions of later variables may refer to previously defined
  variables.
* However, the definition of a variable in a ``let`` may not refer to
  itself; only top-level definitions may be recursive.

Here is a (somewhat contrived) example which demonstrates all these
features:

.. literalinclude:: example/let.disco
   :language: idris
   :caption:

An important thing to note is that a given definition in a ``let``
expression will only ever be evaluated (at most) once, even if the
variable is used multiple times.  ``let`` expressions are thus a way
for the programmer to ensure that the result of some computation is
shared. ``let x = e in f x x`` and ``f e e`` will always yield the
same result, but the former might be more efficient, if ``e`` is
expensive to calculate.

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
multiplicative terms, so ``3x``, ``(-2)x``, and ``(x + 5)x`` all get
parsed as multiplication.  On the other hand, an expression like ``(x
y)`` is always parsed as function application, even if x and y both
turn out to have numeric types; a bare variable like ``x`` does not count
as a multiplicative term.  Likewise, ``(x y) z`` is parsed as function
application, since ``(x y)`` is not a multiplicative term.

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

Operator functions
==================

Operators can be manipulated as functions using the ``~`` notation.
The tilde goes wherever the argument to the operator would go.  This
can be used, for example, to pass an operator to a higher-order
function.

::

    Disco> :type ~+~
    ~+~ : ℕ × ℕ → ℕ

    Disco> import list
    Loading list.disco...
    Disco> foldr(~+~,0,[1 .. 10])
    55

    Disco> -- factorial
    Disco> :type ~!
    ~! : ℕ → ℕ

    Disco> -- negation
    Disco> :type -~
    -~ : ℤ → ℤ
