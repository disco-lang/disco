Propositions
============

``Prop`` is the type of *propositions*.  A proposition is a statement
that could be true or false.  ``Prop`` is very similar to ``Bool``;
the main difference is that whereas any ``Bool`` can always be
evaluated to see if it is ``true`` or ``false``, this may not be
possible for a ``Prop`` due to the use of quantifiers (``forall`` and
``exists``).

Any :doc:`Boolean <bool>` expression can be used as a proposition.
For example, ``true``, ``x > 5``, and ``(x < y) -> ((x == 2) \/ (x ==
3))`` can all be used as propositions.

However, unlike Boolean expressions, propositions can use :doc:`quantifiers
<quantifier>`, that is, ``forall`` or ``exists``.  For example,
``forall x : Z. x^2 >= 0`` is a ``Prop`` (but not a ``Bool``).

If you type a proposition at the Disco prompt, it will simply print
``<Prop>`` and refuse to evaluate it.  If you want to get an idea of
whether a proposition is true or false, you have two options:

- The built-in ``holds`` function tries its best to determine whether
  a proposition is true or false.  If it returns a value at all, it is
  definitely correct.  For example,
    - ``holds ((5 < 3) \/ ((2 < 1) -> false))`` yields ``true``
    - ``holds (forall p:Bool. forall q:Bool. (p \/ q) <-> (q \/ p))``
      yields ``true``
    - ``holds (forall p:Bool. forall q:Bool. (p \/ q) <-> (p /\ q))``
      yields ``false``
    - ``holds (forall n:N. n < 529)`` yields ``false``
  However, sometimes it may simply get stuck in an infinite loop.  For
  example, ``holds (forall n:N. n >= 0)`` will simply get stuck. Even
  though it is obvious to us that this proposition is true, ``holds``
  is not smart enough to see this; it simply tries evaluating ``n >=
  0`` for every natural number, which will never finish.

- One can also use the ``:test`` command.  This command makes a best
  effort to evaluate a proposition, but only trying a finite number of
  examples for infinite domains.  Additionally, in many cases it can
  output much more information than a simple ``true`` or ``false``,
  for example, showing a counterexample if a ``forall`` is false, or a
  witness if an ``exists`` is true.  For example,
    - ``:test forall p:Bool. forall q:Bool. (p \/ q) <-> (p /\ q)``
      prints

        ::

             - Certainly false: ∀p. ∀q. p \/ q <-> p /\ q
               Counterexample:
                 p = false
                 q = true

    - However, ``:test forall n:N. n < 529`` prints

        ::

             - Possibly true: ∀n. n < 529
               Checked 100 possibilities without finding a counterexample.

      Obviously this proposition is false, but Disco apparently does
      not try a big enough value of ``n`` to be able to tell.

Note that at the moment it is not possible to combine propositions
using :doc:`logical operators <logic-ops>` like ``/\``, ``\/``, or
``->``. For example, Disco does not currently accept something like
``(forall x : N. x >= 0) /\ (exists x : N. x == 3)``, although this is
certainly a proposition from a mathematical point of view.  This is
something that may be added in the future.

