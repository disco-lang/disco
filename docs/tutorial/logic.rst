
*****
Logic
*****

Booleans
========

The type of booleans, written ``B`` or ``Bool``, represents logical truth
and falsehood.  The two values of this type are written ``true`` and
``false``. (For convenience ``True`` and ``False`` also work.)

* Logical AND can be written ``and``, ``&&``, or ``∧``.
* Logical OR  is written ``or``, ``||``, or ``∨``.
* Logical negation (NOT) is written ``not`` or ``¬``.

::

    Disco> true and false
    false
    Disco> true || false
    true
    Disco> not (true ∧ true)
    false
    Disco> ¬ (false or false or false or true)
    false

Equality testing
================

If you have two disco values of the type, in almost all cases you can
compare them to see whether they are equal using ``=``, resulting in a
``Bool`` value.

::

    Disco> 2 = 5
    false
    Disco> 3 * 7 = 2*10 + 1
    true
    Disco> (3/5)^2 + (4/5)^2 = 1
    true
    Disco> false = false
    true

The ``/=`` operator tests whether two values are *not* equal; it is
just the logical negation of ``=``.

Comparison
==========

Again, in almost all cases values can be compared to see which is less
or greater, using operators ``<``, ``<=``, ``>``, or ``>=``.

::

    Disco> 2 < 5
    true
    Disco> false < true
    true
    Disco> (5 : Z7) < (9 : Z7)
    false

(The last example is ``false`` because ``(9 : Z7)`` is equivalent to
``(2 : Z7)``.)

Comparisons can also be chained; the result is obtained by comparing
each pair of values according to the comparison between them, and
taking the logical AND of all the results. For example:

::

    Disco> 1 < 3 < 8 < 99
    true
    Disco> 2.2 < 5.9 > 3.7 < 8.8 > 1.0 < 9
    true
