Type definitions
================

Disco has many built-in types, such as ``N``, ``Bool``, and so on.  We
can also define our own types, using the built-in types as
:doc:`building blocks <algebraic-types>`.

Type synonyms
-------------

The basic syntax for a type definition is

::

   type <Name> = <type>

The ``<Name>`` can be whatever name you want, except that it *must
start with a capital letter*.  ``<type>`` can be any type expression.

One simple way to use this facility is to define synonyms for existing
types, for example:

::

   type Count = N

   f : Bool -> Count
   f(True) = 1
   f(False) = 0

In the above example, we define ``Count`` as a synonym for ``N``; we
may then use ``Count`` anywhere in place of ``N``.  Perhaps certain
numbers represent some kind of count and we want to help ourselves
document and remember what these numbers mean.

Somewhat more interestingly, we may define new names as abbreviations
for more :doc:`complex types <algebraic-types>`.  For example:

::

   type OneOrTwo = N + N*N

   ensureTwo : OneOrTwo -> N*N
   ensureTwo(left(n)) = (n,n)
   ensureTwo(right(p)) = p

Here we define ``OneOrTwo`` as a synonym for the type ``N + N*N``,
representing :doc:`either <sum-type>` a single natural number or a
:doc:`pair <product-type>` of natural numbers.

Recursive type definitions
--------------------------

Defining type names as synonyms for other types is convenient but does
not fundamentally add anything to the language; we could always simply
use the original type in place of the new name everywhere.  However,
type definitions can also be *recursive*, that is, types can be
defined in terms of themselves, and this does introduce something
fundamentally new to the language.

As a simple first example, consider the definition of the type
``Chain`` below:

::

   type Chain = Unit + Chain

   chain0 : Chain
   chain0 = left(unit)

   chain1 : Chain
   chain1 = right(left(unit))

   chain2 : Chain
   chain2 = right(right(left(unit)))

XXX more examples: lists, trees

Parameterized type definitions
------------------------------
