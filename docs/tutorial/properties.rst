
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
variables.  The syntax consists of the word ``forall`` (or the Unicode
symbol ``∀``) followed by one or more comma-separated *bindings*
