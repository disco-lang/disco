
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

.. admonition:: To be written

   - Introduce logic operators (and, or, not, implies, iff)
   - Mention `:table` command
   - Introduce basic tests
   - Exercises:
     - xor
