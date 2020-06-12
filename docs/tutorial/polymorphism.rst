
************
Polymorphism
************

Disco includes support for parametric polymorphism, with syntax
similar to Haskell.  For example, here is how we could write a
polymorphic list map function (although ``map`` is actually built in
to Disco; see the next section on containers).

.. literalinclude:: example/poly.disco
   :language: idris
   :caption:

Disco can also infer polymorphic types.  For example:

::

   Disco> :type \x y. x
   λ x y. x : a1 → a → a1
   Disco> :load example/poly.disco
   Loading poly.disco...
   Loaded.
   Disco> :type maplist (\x.x)
   maplist (λx. x) : List a → List a
   Disco> :type maplist (\x.x) [1, 2, 3]
   maplist (λx. x) [1, 2, 3] : List ℕ

However, although Disco has an internal notion of type qualifiers
(like Haskell type classes), these will never show up in inferred
types.  For example:

::

   Disco> :type \x y. x + y
   λ x y. x + y : ℕ → ℕ → ℕ

Internally, Disco is happy to use ``\x y. x + y`` at any type which
supports addition, but when forced to infer a concrete type for it, it
simply picks a suitable monomorphic instantiation.  However, the
following example shows that it can in fact be used on, say, rational
numbers:

::

   Disco> :type (\x y. x + y) (3/2) (-5)
   (λ x y. x + y) (3 / 2) (-5) : ℚ

