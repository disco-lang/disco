
****************
Type Definitions
****************

The ``type`` keyword can be used to conveniently declare aliases for
types.  For example, consider the following function which takes a
list of natural number triplets and returns the sum of all the
triplets in the list:

.. literalinclude:: example/tydefs.disco
   :lines: 3-5
   :language: idris
   :caption:

::

   Disco> sumTripletList [(1,2,3), (4,5,6)]
   21

Let's write the following type definition:

::

   type NatTriple = N * N * N

The type ``NatTriple`` is defined as a 3-tuple containing three values
of type ``N``.  Note that in Disco, all type names must begin with a
capital letter. Now we can rewrite our type declaration for
``sumTripletList`` as follows:

::

   sumTripletList : List NatTriple -> N


Recursive type definitions
==========================

However, ``type`` definitions are in fact much more powerful.  Disco
has no special syntax for declaring algebraic data types as in
Haskell, but unlike Haskell, ``type`` definitions in Disco can be
recursive.  Thus, we can build recursive algebraic data types
directly.  For example, we can define a type of binary trees with
values of type ``N`` at nodes as follows:

.. literalinclude:: example/tydefs.disco
   :lines: 7
   :language: idris
   :caption:

Here, we see that a ``Tree`` can either be a leaf, or a a triplet
containing a natural number value of the root as the first element,
and the left and right subtrees of type ``Tree`` as the second and
third elements, respectively.

Given this definition of ``Tree``, here is how we would write a
function which takes a ``Tree`` and returns the sum of all its node
values.

.. literalinclude:: example/tydefs.disco
   :lines: 9-11
   :language: idris
   :caption:

Recursive type definitions can behave surprisingly at times.  In
essence, two types are considered equivalent as long as they can never
be distinguished no matter how far you unroll their definitions.  This
means, for example, that we can declare the following strange infinite
stream type, consisting of a pair of natural numbers followed by a
stream, and work with it as if it had only a single natural number
followed by a stream---since these two types would be
indistinguishable.

.. literalinclude:: example/tydefs.disco
   :lines: 13-37
   :language: idris
   :caption:

Parameterized type definitions
==============================

Type definitions can also be parameterized. For example, we can make
the ``Tree`` type polymorphic:

.. literalinclude:: example/tydefs-poly.disco
   :language: idris
   :caption:

::

   Disco> :load example/tydefs-poly.disco
   Disco> t = right (1, right (3, left (), left ()), right (5, left (), left ())) : Tree N
   Disco> sumTree(t)
   9
   Disco> flattenTree(t)
   [3, 1, 5]

Cyclic type definitions
=======================

There is only one restriction on recursive ``type`` definitions,
namely, they are not allowed to be cyclic, *i.e.* unguardedly
recursive.  A ``type`` definition is cyclic if the following two
conditions hold:

1. Repeated expansions of the ``type`` definition yield solely ``type`` definitions.

2. The same ``type`` definition is encountered twice during repeated expansion.

For example:

::

   -- Foo is not allowed because it expands to itself.
   type Foo = Foo
   -- Bar is not allowed: it expands to Baz which expands to Bar.
   type Bar = Baz
   type Baz = Bar

   -- Pair is OK because it expands to a top-level type former
   -- (product) which is not a type definition.
   type Pair = Pair * Pair
