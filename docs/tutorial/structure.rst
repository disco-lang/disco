
****************
Structural types
****************

In addition to the primitive types covered so far, disco also has sum
and product types which can be used to build up more complex
structures out of simpler ones.

Product types
=============

The product of two types, written using an asterisk ``*`` or Unicode
times symbol ``×`` (``U+00d7 MULTIPLICATION SIGN``), is a type whose
values are ordered pairs of values of the component types.  Pairs are
written using standard ordered pair notation.

.. literalinclude:: example/pair.disco
   :language: idris
   :caption:

``pair1`` in the example above has type ``N * Q``, that is, the type
of pairs of a natural number and a rational number; it is defined to
be the pair containing ``3`` and ``-5/6``.  ``pair2`` has type ``Z ×
Bool`` (using the alternate syntax ``×`` in place of ``*``), and
contains two values: :math:`17 + 22`, and the result of
asking whether :math:`(3,5) < (4,2)`.

::

    Disco> pair2
    (39, true)

Pairs are compared lexicographically, which intuitively means that the
first component is most important, the second component breaks ties in
the first component, and so on.  For example, :math:`(a,b) < (c,d)` if
either :math:`a < c` (in which case :math:`b` and :math:`d` don't
matter) or if :math:`a = c` and :math:`b < d`.  This is why ``(3,5) <
(4,2)`` evaluates to ``true``. Of course, two pairs are equal exactly
when their first elements are equal and their second elements are
equal.

``pair3`` shows that pairs can be nested: it is a pair whose second
component is also a pair.  ``pair4`` looks like an ordered triple, but
in fact we can check that ``pair3`` and ``pair4`` are equal!

::

    Disco> pair3 == pair4
    true

Really, ``pair4`` is just syntax sugar for ``pair3``.  In general:

* The type ``X * Y * Z`` is interpreted as ``X * (Y * Z)``.

* The tuple ``(x,y,z)`` is interpreted as ``(x,(y,z))``.

This continues recursively, so, for example, ``A * B * C * D * E``
means ``A * (B * (C * (D * E)))``.  Put another way, disco really only
has pairs, but appears to support arbitrarily large tuples by encoding
them as right-nested pairs.

If you want *left*-nested pairs you can use explicit parentheses: for
example, ``(Bool * Bool) * Bool`` is not the same as ``Bool * Bool *
Bool``, and has values such as ``((false, true), true)``.

Sum types
=========

If ``X`` and ``Y`` are types, their *sum*, written ``X + Y`` (or ``X ⊎
Y``, using ``U+228e MULTISET UNION``), is the disjoint union of ``X``
and ``Y``.  That is, values of type ``X + Y`` are either values of
``X`` or values of ``Y``, along with a "tag" so that we know which it
is.  The possible tags are ``left`` and ``right`` (to indicate the
type on the left or right of the ``+``).  For example:

.. literalinclude:: example/sum.disco
   :language: idris
   :caption:

``sum1`` and ``sum2`` have the same type, namely, ``N + Bool``; values
of this type consist of either a natural number or a boolean.
``sum1`` contains a natural number, tagged with ``left``; ``sum2``
contains a boolean tagged with ``right``.

Notice that ``X + X`` is a different type than ``X``, because we get
two distinct copies of all the values in ``X``, some tagged with
``left`` and some with ``right``. This is why we call a sum type a
*disjoint* union.

Iterated sum types, as in ``sum3``, are handled in exactly the same
way as iterated product types: ``N + N + N`` is really syntax sugar
for ``N + (N + N)``.  ``sum3`` therefore begins with a ``right`` tag,
to show that it contains a value of the right-hand type, namely, ``N +
N``; this value in turn consists of another ``right`` tag along with a
value of type ``N``.  Other values of the same type ``N + N + N``
include ``right (left 6)`` and ``left 5``.

Unit and Void types
===================

Disco has two other special built-in types which are rarely useful on
their own, but often play an important role in describing other types.

* The type ``Unit`` has just a single value, called ``()``.

    ::

        Disco> :type ()
        () : Unit

* The type ``Void`` has *no* values.

Counting and enumerating types
==============================

For any type which has only a finite number of values, disco can count
how many values there are, using the ``count`` operator, or list them
using ``enumerate`` (we will learn more about lists later in the
tutorial).

::

    Disco> count ((Bool * (Bool + Bool)) + Bool)
    right 10
    Disco> enumerate ((Bool * (Bool + Bool)) + Bool)
    [left (false, left false), left (false, left true), left (false, right false),
     left (false, right true), left (true, left false), left (true, left true),
     left (true, right false), left (true, right true), right false, right true]
    Disco> enumerate (Bool * Bool * Bool)
    [(false, false, false), (false, false, true), (false, true, false), (false, true, true),
     (true, false, false), (true, false, true), (true, true, false), (true, true, true)]
