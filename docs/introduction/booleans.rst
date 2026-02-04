
Booleans and Logic
==================

In Disco, the type ``Bool`` has two values, ``T`` and ``F`` (which can
also be written ``True`` and ``False``, or ``true`` and ``false``):

::

    Disco> :type T
    T : Bool
    Disco> :type F
    F : Bool
    Disco> true
    T
    Disco> False
    F

Comparison operators
--------------------

One way we can create ``Bool`` values in Disco is using :doc:`comparison
operators </reference/compare>`.

- The ``<`` (less than) operator checks whether one number is strictly
  less than another:

    ::

       Disco> 3 < 5
       T
       Disco> 5 < 5
       F
       Disco> 7 < 5
       F

- The ``<=`` (less than or equal to) operator checks whether one
  number is less than or equal to another:

    ::

       Disco> 3 <= 5
       T
       Disco> 5 <= 5
       T
       Disco> 7 <= 5
       F

- ``>`` (greater than) and ``>=`` (greater than or equal to) work similarly:

    ::

       Disco> 3 > 5
       F
       Disco> 5 > 5
       F
       Disco> 7 > 5
       T
       Disco> 3 >= 5
       F
       Disco> 5 >= 5
       T
       Disco> 7 >= 5
       T

- Finally, ``==`` can be used to check whether two things are equal,
  and ``!=`` (or ``/=``) check whether two things are not equal:

    ::

       Disco> 3 == 5
       F
       Disco> 5 == 5
       F
       Disco> 3 /= 5
       T
       Disco> 5 /= 5
       F

Notice that Disco requires a double equals symbol, ``==``, to check
whether two things are equal, rather than ``=``.  If you're curious,
the reason for this is explained on the page about :doc:`definition vs
equality testing </reference/def-vs-test>`.

Boolean operators
-----------------

Disco has a number of *operators* that can be used to manipulate and
combine Boolean values.

* The ``not`` operator (which can also be written ``¬``) flips ``T``
  to ``F`` and vice versa.

    ::

       Disco> not T
       F
       Disco> not F
       T
       Disco> ¬ true
       F

  We can also use the ``:table`` command to ask Disco to print out a
  table showing all the possible values of a function or operator:

    ::

       Disco> :table not
       F  T
       T  F

* The ``and`` operator (which can also be written ``&&`` or ``/\`` or
  ``∧``) implements logical conjunction: the result is only ``T`` if
  both inputs are ``T``.

    ::

       Disco> :table and
       F  F  F
       F  T  F
       T  F  F
       T  T  T

* The ``or`` operator (also written ``||`` or ``\/`` or ``∨``)
  implements logical disjunction: the result is only ``T`` if at least one
  of the inputs is ``T``.

    ::

       Disco> :table or
       F  F  F
       F  T  T
       T  F  T
       T  T  T

* The ``implies`` operator (also written ``->``) implements logical
  implication: the result is only ``F`` when the left-hand side is
  ``T`` but the right-hand side is ``F``, and it is ``T`` otherwise.

    ::

       Disco> :table implies
       F  F  T
       F  T  T
       T  F  F
       T  T  T

* Finally, there is an ``iff`` operator (also written ``<->``) which
  is true when its two inputs are the same, and false otherwise.

    ::

       Disco> :table iff
       F  F  T
       F  T  F
       T  F  F
       T  T  T

Tests
-----

- Introduce basic tests: syntax, `:test` command

Exercises
---------

- Exercises:
  - xor
