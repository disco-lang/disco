Comparison
==========

*Comparison operators* can be used to compare two values, to test if
they are equal or to see what order they are in.  Comparison operators
always return a :doc:`boolean <bool>` value.

Values of almost any type can be compared: :doc:`numeric types
<numeric>`, :doc:`booleans <bool>`, :doc:`characters <char>`,
:doc:`strings <string>`, and any :doc:`pair <product-type>`,
:doc:`sum <sum-type>`, or :doc:`collection <collections>` types built out
of these.  For example, sets of pairs of natural numbers and strings
can be compared:

::

   Disco> {(3, "hi"), (4, "there"), (6, "world")} < {(10, "what")}
   T

See the individual pages about each type for more information on how
comparison works on values of that type.

:doc:`Functions <function-types>`, on the other hand, cannot be
compared, because in general this would require testing the functions
on every single possible input, of which there might be infinitely
many.

::

   Disco> (\n:N. n) < (\n:N. n+1)
   Error: values of type a2 → a3 cannot be compared.
   https://disco-lang.readthedocs.io/en/latest/reference/not-qual.html

* The ``==`` operator is for testing equality.  For example,

    ::

       Disco> 3 == 5
       F
       Disco> "hi" == "hi"
       T

  Equality is one critical point where Disco syntax has to deviate
  from standard mathematical notation: be sure to keep in mind the
  difference between ``=`` (which is used to :doc:`define things
  <definition>`) and ``==``, used for testing whether two things are
  equal.  For more information on the difference, see the page on
  :doc:`definition vs equality testing <def-vs-test>`.

* The ``/=`` operator (also written ``≠`` or ``!=``) means "not equal
  to".  It is true if and only if its two arguments are not equal. The
  syntax ``/=`` is supposed to remind us of the standard mathematical
  notation of an equality sign with a slash through it (``≠``).
  However, ``!=`` is also provided for those used to this operator in
  other programming languages.

* There are four operators that can be used to test the ordering of
  two values:

    - ``<`` tests whether the first value is less than the second.
    - ``>`` tests whether the first value is greater than the second.
    - ``<=``, also written ``≤`` or ``=<``, tests whether the first
      value is less than or equal to the second.
    - ``>=``, also written ``≥`` or ``=>``, tests whether the first
      value is greater than or equal to the second.

* You can chain multiple comparisons; this always means the same thing
  as combining all the individual comparisons with "and".  For
  example, ``3 <= x < y <= 10`` means the same thing as ``3 <= x /\ x
  < y /\ y <= 10``.

* The ``max`` and ``min`` functions can be used on any type which
  supports comparison.  ``max`` takes two arguments and returns
  whichever one is larger; ``min`` is similar but returns the smaller
  argument. For example, ``max(3,6)`` and ``max(6,3)`` both evaluate
  to ``6``.
