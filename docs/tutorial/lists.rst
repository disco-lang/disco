
*****
Lists
*****

Disco defines a type of inductive, singly-linked lists, very similar
to lists in Haskell.

Basic lists
===========

All the elements of a list must be the same type, and the type of a
list with elements of type ``T`` is written ``List T``.  Since it is
unambiguous, nested list types can be written without parentheses,
*e.g.* ``List List List T``.

The basic syntax for constructing and pattern-matching on lists is
almost exactly the same as in Haskell, with the one difference that
the single colon (type of) and double colon (cons) have been switched
from Haskell.

.. literalinclude:: code/list-example.disco
   :language: idris
   :caption:

List comprehensions
===================

Disco has list comprehensions which are also similar to Haskell's.  A
list comprehension is enclosed in square brackets, and consists of an
expression, followed by a vertical bar, followed by zero or more
*qualifiers*, ``[ <expr> | <qual>* ]``.

A *qualifier* is one of:

* A *binding* qualifier of the form ``x in <expr>``, where ``x`` is
  a variable and ``<expr>`` is any expression with a list type.  ``x``
  will take on each of the items of the list in turn.

* A *guard* qualifier, which is an expression with a boolean type.  It
  acts to filter out any bindings which cause the expression to
  evaluate to ``false``.

For example, ``comp1`` below is a (rather contrived) function on two
lists which results in all possible sums of two *even* numbers taken
from the lists which add to at least 50.  ``pythagTriples`` is a list
of all Pythagoren triples with all three components at
most 100. (There are much more efficient ways to compute Pythagorean
triples, but never mind.)

.. literalinclude:: code/comprehension-example.disco
   :language: idris
   :caption:

.. note::

    The biggest difference between list comprehensions in disco and
    Haskell is that Haskell allows *pattern* bindings, *e.g.*  ``Just x <-
    xs``, which keep only elements from the list which match the
    pattern.  At the moment, disco only allows variables on the
    left-hand side of a binding qualifier.  There is no reason in
    principle disco can't support binding qualifiers with patterns, it
    just isn't a big priority and hasn't been implemented yet.

Polynomial sequences
====================

Like Haskell, disco supports ellipsis notation in literal lists to
denote omitted elements, although there are a few notable
differences.  One minor syntactic difference is that (just for fun)
disco accepts two *or more* dots as an ellipsis; the number of dots
makes no difference.

.. literalinclude:: code/basic-ellipsis-example.disco
    :language: idris
    :caption:

* ``[a ..]`` denotes the infinite list beginning with ``a`` and
  counting up by ones.
* ``[a .. b]`` denotes the list that starts with ``a`` and either
  counts up or down by ones (depending on whether ``b`` is greater
  than or less than ``a``, respectively), continuing as long as the
  elements do not "exceed" ``b`` (the meaning of "exceed" depends on
  whether the counting is going up or down).
* ``[a, b .. c]`` denotes the list whose first element is ``a``,
  second element is ``b``, and the difference between each element and
  the next is the difference ``b - a``.  The list continues as long as
  the elements do not "exceed" ``c``, where "exceed" means either
  "greater than" or "less than", depending on whether ``b - a`` is
  positive or negative, respectively.
* ``[a, b ..]`` is similar but infinite.

All the above is similar to Haskell, except that ``[10 .. 1]`` is the
empty list in Haskell, and disco's rules about determining when the
list stops are much less strange (the strangeness of Haskell's rules
is occasioned by floating-point error, which of course disco does not
have to deal with).

However, disco also generalizes things further by allowing notation of
the form ``[a, b, c ..]`` or ``[a, b, c, d ..]``, and so on.  We have
already seen that two values ``[a, b ..]`` generate a linear
progression of values; by analogy, three values generate a quadratic
progression, four values a cubic, and so on.  In general, when
:math:`n` values :math:`a_0, a_1, \dots, a_n` are given before an
ellipsis, disco finds the unique polynomial :math:`p` of degree
:math:`n-1` such that :math:`p(i) = a_i`, and uses it to generate
additional terms of the list. (In practice, the disco interpreter does
not actually find a polynomial, but uses the *method of finite
differences*, just like Charles Babbage's Difference Engine.)

.. literalinclude:: code/general-ellipsis-example.disco
    :language: idris
    :caption:

When an ending value is specified, list elements are again included
until the first one which "exceeds" the ending value.  The precise
definition of "exceeds" is a bit trickier to state in general, but
corresponds to the eventual behavior of the polynomial: the list stops
as soon as elements become either larger than or smaller than the
ending value, as the polynomial diverges to :math:`+\infty` or
:math:`-\infty`, respectively.

Multinomial coefficients
========================

We already saw that the ``choose`` operator can be used to compute
binomial coefficients.  In fact, if the second operand to ``choose``
is a list instead of a natural number, it can be used to compute
general multinomial coefficients as well.  ``n choose xs`` is the
number of ways to choose a sequence of sets whose sizes are given by
the elements of ``xs`` from among a set of ``n`` items.  If the sum of
``xs`` is equal to ``n``, then this is given by ``n!`` divided by the
product of the factorials of ``xs``; if the sum of ``xs`` is greater
than ``n``, then ``n choose xs`` is zero; if the sum is less than
``n``, it is as if another element were added to ``xs`` to make up the
sum (representing the set of elements which are "not chosen").  In
general, ``n choose k = n choose [k,n-k] = n choose [k]``.
