Comprehensions
==============

*Comprehension* notation can be used to describe collections such as
:doc:`sets <set>` or :doc:`lists <list>`.  The general syntax for a
set comprehension is

::

   { expression | qualifiers }

with a single expression, followed by a vertical bar ``|``, followed
by a list of one or more *qualifiers*.  The idea is introduced through
some examples below; for the precise details, see the
`Details`_ section.

:doc:`List <list>` comprehensions are similar, but use square brackets
(``[``, ``]``) instead of curly braces (``{``, ``}``).

Examples
--------

::

   Disco> {x | x in {1..5}}   -- same as {1..5}
   {1, 2, 3, 4, 5}
   
   Disco> {3x | x in {1..5}}  -- multiply each element of {1..5} by 3
   {3, 6, 9, 12, 15}
   
   -- Pick out the elements of {1..10} that satisfy the condition
   Disco> {x | x in {1 .. 10}, x^2 + 20 == 9x}
   {4, 5}

   -- Pick out the elements of {1..100} that satisfy all the conditions
   Disco> {x | x in {1 .. 100}, x =< 10 \/ x >= 90, x mod 2 == 0}
   {2, 4, 6, 8, 10, 90, 92, 94, 96, 98, 100}

   -- Products of all combinations of elements from {1..4} and {1, 10, 100}
   Disco> {x * y | x in {1 .. 4}, y in {1, 10, 100}}
   {1, 2, 3, 4, 10, 20, 30, 40, 100, 200, 300, 400}

   -- Pairs of elements from {1..4} where the first is >= the second
   Disco> {(x,y) | x in {1 .. 4}, y in {1 .. x}}
   {(1, 1), (2, 1), (2, 2), (3, 1), (3, 2), (3, 3), (4, 1), (4, 2), (4, 3), (4, 4)}

Details
-------

Each *qualifier* in a comprehension can be either

* a *variable binding* of the form ``<variable> in <set>``, *e.g.* ``x
  in {1 .. 10}`` or ``b in {false, true}``, or
* a *guard*, which can be any :doc:`boolean <bool>` expression.

A variable binding locally defines a variable and causes it to "loop" through
all the values in the given set.  For example, ``x in {1 .. 5}``
defines the variable ``x`` within the comprehension, and makes ``x``
take on each value from 1 through 5 in turn.  Multiple variable
bindings will cause the loops to "nest".  For example, ``{ (x,y) | x in {1 .. 3},
y in {5 .. 7}}`` has nine elements: ``y`` loops through its three
possible values for *each* value of ``x``.

::

   Disco> { (x,y) | x in {1 .. 3}, y in {5 .. 7}}
   {(1, 5), (1, 6), (1, 7), (2, 5), (2, 6), (2, 7), (3, 5), (3, 6), (3, 7)}

A boolean guard is checked for each combination of variable values to
see if it is true.  Any values of the variables which make the guard
false are discarded.

Finally, any values of the variable(s) which make all the guards
true are used in the expression on the left side of the ``|``, and the
resulting value will become an element of the set.

Putting all this together, for example, ``{x^2 + y | x in {1 .. 5}, x mod 2 == 1, y in {1 .. x}, x + y > 5}`` is evaluated as follows:

* ``x`` will loop through the values from 1 to 5.
* For each value of ``x``, check whether ``x mod 2 == 1``.  The values
  which make this false (2 and 4) are discarded.  The only values of
  ``x`` left are 1, 3, and 5.
* For each of the remaining values of ``x``, ``y`` will loop through
  the values from 1 up to ``x``.
* For each value of ``y``, check whether the sum of ``x`` and ``y`` is
  greater than 5.
* Finally, from values of ``x`` and ``y`` which make it through both
  checks, we compute ``x^2 + y`` and put the result in the set being
  built.

In the end, the result is the set ``{12, 26, 27, 28, 29, 30}``.

Specification
-------------

.. note::

   In case you are curious about the precise definition and are not
   afraid of the details, the exact way that set comprehensions
   work can be defined by the following three equations, making use of
   the standard functions ``each`` and ``unions``:

   * ``{ e | } = e``
   * ``{ e | x in xs, gs } = unions(each(\x. {e | gs}, xs))``
   * ``{ e | g, gs } = {? { e | gs } if g, {} otherwise ?}``
