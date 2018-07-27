
****************
Type Definitions
****************

Disco supports recursive type definitions. Let's look at a few example.

Consider the following function which takes a list of natural number triplets and
returns the sum of all the triplets in the list:

.. literalinclude:: example/tydefs.disco
   :lines: 3-5
   :language: idris
   :caption:

::  Disco> sumTripletList [(1,2,3), (4,5,6)]
     21

Let's write the following type definition:

::

	type NatTriple = Nat * Nat * Nat

The type ``NatTriple`` is defined as a 3-tuple containing three values of type ``N``.
In Disco, all type definitions must begin with a capital letter. Now we can rewrite
our type declaration for ``sumTripletList`` as follows:

::
	
	sumTripletList : List NatTriple -> Nat


Recursive Type Definitions
==========================

Note that type definitions can be recursive too! Consider the following type definition
for a binary tree with values of type ``N`` at nodes:

::

	type Tree = Nat + (Nat * Tree * Tree)

Here, we see that a ``Tree`` can either be a leaf with a value of type ``Nat`` or a
a triplet containing the ``Nat`` value of the root as the first element, and the left and
right subtrees of type ``Tree`` as the second and third elements, respectively.

Here's a function which takes a ``Tree`` and returns the sum of all it's node values.

.. literalinclude:: example/tydefs.disco
   :lines: 9-11
   :language: idris
   :caption:

Cyclic Type Definitions
=======================

Disco does not support cyclic type definitions. A type definition is cyclic if
the following conditions hold:

1.) Repeated expansions of the type definition yield solely type definitions.

2.) The same type definition is encountered twice during repeated expansion.

Here are a few examples of cyclic type definitions:

::
	-- Foo is cyclic because it expands to itself.
	type Foo = Foo
	-- Bar is cyclic because it expands to Baz which expands to Bar.
	type Bar = Baz
	type Baz = Bar

::
	-- Pair is not cyclic because it expands to a type which is not a type definition (although it's expansion contains type definitions)
	type Pair = (Pair, Pair)

