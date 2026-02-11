
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

We can attach *tests* to Disco functions.  Each test goes on a line by
itself beginning with ``!!!``.  For example:

::

  !!! double(0) == 0
  !!! double(2) == 4
  !!! double(7) == 14
  !!! forall n : N. double(n) >= 0
  double : N -> N
  double(n) = 2n

Tests can either be simple true or false tests, or they can have a
``forall`` to say that something should be true for every value
of a certain type.  When we load this file, Disco will run
the tests and report whether they succeed.

::

  Disco>
  Loading double.disco...
  Running tests...
    double: OK
  Loaded.

If we try changing the ``double(n) >= 0`` test to
``double(n) > 0``, we can see that the test fails:

::

  Disco>
  Loading double.disco...
  Running tests...
    double:
    - Certainly false: ∀n. double(n) > 0
      Found counterexample:
        n = 0
      - Left side:  0
      - Right side: 0
  Loaded.

However, another way to express a correct test would be as follows:

::

  !!! forall n : N. (n == 0) or double(n) > 0

You can also run tests directly at the Disco prompt with the ``:test``
command.  For example:

::

  Disco> :test 2 + 3 == 5
    - Certainly true: 2 + 3 == 5
      - Left side:  5
      - Right side: 5
  Disco> :test 2 * 3 == 5
    - Certainly false: 2 * 3 == 5
      - Left side:  6
      - Right side: 5
  Disco> :test forall b:Bool. (b /\ b) == b
    - Certainly true: ∀b. (b /\ b) == b
      No counterexamples exist; all possible values were checked.
  Disco> :test forall n:N. n^2 < 10
    - Certainly false: ∀n. n ^ 2 < 10
      Found counterexample:
        n = 4
      - Left side:  16
      - Right side: 10

Exercises
---------

Complete the definition of the ``xor`` function below.

::

  ||| xor computes the *exclusive or* of its inputs; it is true when
  ||| exactly one of its inputs is true. That is, xor(p,q) should be
  ||| true when one or the other of p or q is true, but false if either
  ||| both p and q are false, or both p and q are true.

  !!! xor(false, false) == false
  !!! xor(false, true ) == true
  !!! xor(true , false) == true
  !!! xor(true , true ) == false

  xor : Bool * Bool -> Bool    -- This says that xor takes a pair of true/false values as input
                               --   and returns one as output

  xor(p,q) = $crash "xor is not defined!"
    -- Delete $crash and the following message above, and replace it
    -- with a definition of xor, making use of other built-in operators in Disco
    -- such as /\, \/, ->, not.  You will know that you were successful if :loading
    -- this file at the Disco prompt gives a message saying "xor: OK".
