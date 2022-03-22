The type T is not searchable
============================

When writing a :doc:`property <property>` using a ``forall``, we are
only allowed to use types which are *searchable*, that is, types for
which we can effectively generate sample values.  Note this is not the
same thing as being finite.  For example, the type of natural numbers
is searchable even though it is infinite; we can list natural numbers
(either in order or randomly) in order to see which ones make the
property true or false.

::

   Disco> :test forall x:N. x < 10    -- this works as expected
     - Test is false: ∀x. x < 10
       Counterexample:
         x = 10

On the other hand, the :doc:`function <function>` type `N -> N` is not
searchable: there is no way to list all functions of type `N -> N`.
The below property is in fact false, but Disco can't handle it:

::

   Disco> :test forall f : N -> N. f(4) > 6
   Error: the type
     ℕ → ℕ
   is not searchable (i.e. it cannot be used in a forall).

This error can also occur when you forget to put a type on a variable
in a ``forall``.  For example:

::

   Disco> :test forall x. x == x
   Error: the type
     a1
   is not searchable (i.e. it cannot be used in a forall).

The problem here is really that Disco does not know what type ``x``
should be.  If we add a type annotation on ``x``, it works fine:

::

   Disco> :test forall x: Bool. x == x
     - Test passed: ∀x. x == x
       No counterexamples exist.
   Disco> :test forall x: N. x == x
     - Test passed: ∀x. x == x
       Checked 100 possibilities without finding a counterexample.

