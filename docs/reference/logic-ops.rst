Logical operations
==================

* Logical negation is written ``not``; it inverts ``true`` to
  ``false`` and vice versa.
* Logical conjunction, aka AND, is written ``/\``, ``and``, or
  ``&&``.  It has the following truth table:

    =====  =====  ==========
    ``x``  ``y``  ``x /\ y``
    =====  =====  ==========
    F      F      F
    F      T      F
    T      F      F
    T      T      T
    =====  =====  ==========

* Logical disjunction, aka OR, is written ``\/``, ``or``, or
  ``||``. It has the following truth table:

    =====  =====  ==========
    ``x``  ``y``  ``x \/ y``
    =====  =====  ==========
    F      F      F
    F      T      T
    T      F      T
    T      T      T
    =====  =====  ==========

* Logical implication, aka IF-THEN, is written ``->``, ``==>``, or
  ``implies``. It has the following truth table:

    =====  =====  ==========
    ``x``  ``y``  ``x -> y``
    =====  =====  ==========
    F      F      T
    F      T      T
    T      F      F
    T      T      T
    =====  =====  ==========
