Comprehensions
==============

*Comprehension* notation can be used to describe collections such as
:doc:`sets <set>` or :doc:`lists <list>`.  The general syntax for a
set comprehension is

::

   { expression | qualifiers }

with a single expression, followed by a vertical bar ``|``, followed
by a list of one or more *qualifiers*.  The idea is introduced through
a bunch of examples below; for the precise details, see the
`Details`_ section.

Examples
--------

::

   Disco> {x | x in {1..5}}
   {1, 2, 3, 4, 5}
   Disco> {3x | x in {1..5}}
   {3, 6, 9, 12, 15}
   Disco> {x | x in {1 .. 10}, x^2 + 20 == 9x}
   {4, 5}
   Disco> import num
   Disco> {x | x in {1 .. 100}, x =< 10 \/ x >= 90, even x}
   {2, 4, 6, 8, 10, 90, 92, 94, 96, 98, 100}
   Disco> {x * y | x in {1 .. 4}, y in {1, 10, 100}}
   {1, 2, 3, 4, 10, 20, 30, 40, 100, 200, 300, 400}
   Disco> {(x,y) | x in {1 .. 4}, y in {1 .. x}}
   {(1, 1), (2, 1), (2, 2), (3, 1), (3, 2), (3, 3), (4, 1), (4, 2), (4, 3), (4, 4)}

Details
-------

Each *qualifier* in a comprehension can be either

* a *variable binding* of the form ``<variable> in <set>``, *e.g.* ``x
  in {1 .. 10}`` or ``b in {false, true}``, or
* a *guard*, which can be any :doc:`boolean <bool>` expression.
