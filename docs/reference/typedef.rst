Type definitions
================

Disco has many built-in :doc:`types <types>`, such as ``N``, ``Bool``, and so on.  We
can also define our own types, using the built-in types as
:doc:`building blocks <algebraic-types>`.

Type synonyms
-------------

The basic syntax for a type definition is

::

   type <Name> = <type>

The ``<Name>`` can be whatever name you want, but it *must
start with a capital letter*.  ``<type>`` can be any type expression.

One simple way use this is to define synonyms for existing types. For
example:

::

   type Count = N

   f : Bool -> Count
   f(True) = 1
   f(False) = 0

In the above example, we define ``Count`` as a synonym for ``N``; we
may then use ``Count`` anywhere we want in place of ``N``.  Perhaps
certain numbers represent some kind of count and we want to help
ourselves document and remember what these numbers mean.

Somewhat more interestingly, we may define new names as abbreviations
for more :doc:`complex types <algebraic-types>`.  For example:

::

   type Two = N*N
   type OneOrTwo = N + Two

   ensureTwo : OneOrTwo -> Two
   ensureTwo(left(n)) = (n,n)
   ensureTwo(right(p)) = p

Here we define ``OneOrTwo`` as a synonym for the type ``N + Two``
(which in turn means ``N + N*N``), representing :doc:`either
<sum-type>` a single natural number or a :doc:`pair <product-type>` of
natural numbers.

Recursive type definitions
--------------------------

Defining type names as synonyms for other types is convenient, but
does not fundamentally add anything to the language.  However, type
definitions can also be *recursive*, that is, types can be defined in
terms of themselves. This, it turns out, does give us a fundamentally
new ability.

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

Values of type ``Chain`` are defined as being either a ``Unit`` value
(wrapped in ``left``, like ``chain0``) or another ``Chain`` value
(wrapped in ``right``, like ``chain1`` or ``chain2``).  To make a
``Chain`` value, we can therefore keep choosing ``right`` as long as
we want, until we finally stop with ``left(unit)``.  So values of type
``Chain`` are actually very similar to natural numbers.

As another example, we could define binary trees of rational numbers
like this:

::

   type Tree = Unit + (Tree * Q * Tree)

   leaf : Tree
   leaf = left(unit)

   tree1 : Tree
   tree1 = right(leaf, 1/2, leaf)

   tree2 : Tree
   tree2 = right(tree1, 5/7, leaf)

Parameterized type definitions
------------------------------

It is also possible to create type definitions which have one or more
type *parameters*.  Type parameters are always written in parentheses.
For example,

::

   type Maybe(a) = Unit + a

   type Tree(a) = Unit + (Tree(a) * a * Tree(a))

   type FringyTree(a,b) = b + (FringyTree(a,b) * a * FringyTree(a,b))

When using a parameterized type, you can put whatever type you want in
place of the parameters.  For example,

::

   x : Maybe(N)
   x = right(3)

   t : FringyTree(Bool, Q)
   t = right(left(True), 2/3, left(False))
