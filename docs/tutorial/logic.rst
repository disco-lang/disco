
*****
Logic
*****

Booleans
========

The type of booleans, written ``Bool`` or ``Boolean``, represents
logical truth and falsehood.  The two values of this type are written
``true`` and ``false``. (For convenience ``True`` and ``False`` also
work.)

* Logical AND can be written ``and``, ``&&``, or ``∧`` (note that ``∧``
  is ``U+2227 LOGICAL AND``, not a caret symbol ``^``, which is
  reserved for exponentiation).
* Logical OR  is written ``or``, ``||``, or ``∨`` (``U+2228 LOGICAL OR``).
* Logical negation (NOT) is written ``not`` or ``¬`` (``U+00AC NOT
  SIGN``).
* Logical implication is written ``implies`` or ``==>``.

::

    Disco> true and false
    false
    Disco> true || false
    true
    Disco> not (true ∧ true)
    false
    Disco> ¬ (false or false or false or true)
    false
    Disco> true implies false
    false
    Disco> false implies true
    true

Equality testing
================

If you have two disco values of the same type, in almost all cases you
can compare them to see whether they are equal using ``==``, resulting
in a ``Bool`` value.

::

    Disco> 2 == 5
    false
    Disco> 3 * 7 == 2*10 + 1
    true
    Disco> (3/5)^2 + (4/5)^2 == 1
    true
    Disco> false == False
    true

The ``/=`` operator tests whether two values are *not* equal; it is
just the logical negation of ``==``.

Comparison
==========

Again, in almost all cases values can be compared to see which is less
or greater, using operators ``<``, ``<=``, ``>``, or ``>=``.

::

    Disco> 2 < 5
    true
    Disco> false < true
    true

Comparisons can also be chained; the result is obtained by comparing
each pair of values according to the comparison between them, and
taking the logical AND of all the results. For example:

::

    Disco> 1 < 3 < 8 < 99
    true
    Disco> 2.2 < 5.9 > 3.7 < 8.8 > 1.0 < 9
    true
    Disco> x : Int
    Disco> x = 5
    Disco> 2 < x < 10
    true
