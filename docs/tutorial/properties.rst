
**********
Properties
**********

Each disco definition may have any number of associated *properties*.,
mathematical claims about the behavior of the definition which can be
automatically verified by disco.  Properties begin with ``!!!``, must
occur just before their associated definition, and may be arbitrarily
interleaved with documentation lines beginning with ``|||``.

Unit tests
==========

The simplest kind of property is just an expression of type ``Bool``,
which essentially functions as a unit test.  When loading a file,
disco will check that all such properties evaluate to ``true``, and
present an error message if any do not.

.. literalinclude:: example/unit-test.disco
   :language: idris
   :caption:

When we load this file, disco reports that it successfully ran the
tests associated with ``gcd``:

::

    Disco> :load example/unit-test.disco
    Loading example/unit-test.disco...
    Running tests...
      gcd: OK
    Loaded.

On the other hand, if we change the first property to ``!!! gcd(7,6) =
2`` and load the file again, we get an error:

::

    Disco> :load example/unit-test.disco
    Loading example/unit-test.disco...
    Running tests...
      gcd:
      - Test result mismatch for: gcd (7, 6) = 2
        - Expected: 2
        - But got:  1
    Loaded.

Quantified properties
=====================

More generally, properties can contain universally quantified
variables.  The syntax for a universally quantified property is as
follows:

* the word ``forall`` (or the Unicode symbol ``∀``);
* one or more comma-separated *bindings*, each consisting of a
  variable name, a colon, and a type;
* a period;
* and an arbitrary expression, which should have type ``Bool`` and
  which may refer to the variables bound by the ``forall``.

Such quantified properties have the obvious logical interpretation:
they hold only if the given expression evaluates to ``true`` for all
possible values of the quantified variables.

.. literalinclude:: example/property.disco
   :language: idris
   :caption:

In the example above, the first three properties have a single
quantified variable, and specify respectively that ``neg`` is
self-inverse, and ``plusIso`` and ``plusIsoR`` are inverse.  The last
function has a property with multiple quantified variables, and
specifies that ``f`` is associative.  Notice that as in this last
example, properties may extend onto multiple lines, as long as
subsequent lines are indented. Only a single ``!!!`` should be used at
the start of each property.

Such properties may be undecidable in general, so disco cannot
automatically *prove* them.  Instead, it searches for counterexamples.
If the input space is finite and sufficiently small (as in the first
example above, which quantifies over a single boolean), disco will
enumerate all possible inputs and check each one; so in this special
case, disco can actually prove the property by exhaustively checking
all cases.  Otherwise, disco randomly generates a certain number of
inputs (*a la* ``QuickCheck``) and checks that the property is
satisfied for each.  If a counterexample is found, the property
certainly does not hold, and the counterexample can be printed.  If no
counterexample is found, the property "probably" holds.
