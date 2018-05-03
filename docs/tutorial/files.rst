
******************************
Disco files and the disco REPL
******************************

For anything beyond simple one-off calculations that can be entered at
the disco prompt, disco definitions may be stored in a file which can
be loaded into the REPL.

Disco files
===========

Disco files typically end in ``.disco``.
Here is a simple example:

.. literalinclude:: example/basics.disco
   :language: idris
   :caption:

This file contains definitions for ``approx_pi`` and ``increment``.
Each definition consists of a *type signature* of the form ``<name> :
<type>``, followed by an equality of the form ``<name> =
<expression>``.  Both parts of a definition are required; in
particular, if you omit a type signature, disco will complain that the
name is not defined.  The example file shown above contains two
definitions: ``approx_pi`` is defined to be the ``Rational`` number
:math:`22/7`, and ``increment`` is defined to be the function which
outputs one more than its natural number input. (Functions and the
syntax for defining them will be covered in much more detail in an
upcoming section of the tutorial.)

To load the definitions in a file into the disco REPL, you can use the
``:load`` command.  After successfully loading a file, all the names
defined in the file are available for use.  For example:

::

    Disco> :load example/basics.disco
    Loading example/basics.disco...
    Loaded.
    Disco> approx_pi
    22/7
    Disco> increment 3
    4
    Disco> :type increment
    increment : ℕ → ℕ
    Disco> approx_pi + increment 17
    148/7

Comments and documentation
==========================

Comments in disco can be written in one of two ways, using the same
syntax as Haskell.  Two consecutive hyphens ``--`` will cause disco to ignore
everything until the next newline character; ``{- ... -}`` creates a
multi-line comment causing disco to ignore everything in between
``{-`` and ``-}``.

.. literalinclude:: example/comment.disco
   :language: idris
   :caption:

Comments can be placed anywhere and are literally ignored by disco.
In many cases, however, the purpose of a comment is to provide
documentation for a function.  In this case, disco supports special
syntax for *documentation*, which must be placed before the type
signature of a definition.  Each line of documentation must begin with
``|||`` (three vertical bars).

.. literalinclude:: example/doc.disco
   :language: idris
   :caption:

When this file is loaded into the disco REPL, we can use the ``:doc``
command to see the documentation associated with each name.

::

    Disco> :load example/doc.disco
    Loading example/doc.disco...
    Loaded.
    Disco> :doc approx_pi
    approx_pi : ℚ

    A reasonable approximation of pi.

    Disco> :doc increment
    increment : ℕ → ℕ

    Take a natural number as input, and return the natural
    number which is one greater.

    Should not be used while operating heavy machinery.

    Disco> :doc fizz
    fizz : ℕ

Since ``fizz`` does not have any associated documentation, the
``:doc`` command simply shows its type.

Other REPL commands
===================

The disco REPL has a few other commands which are useful for disco
developers.

* ``:parse`` shows the fully parsed form of an expression.

    ::

        Disco> :parse 2 + [3,4 : Int]
        TBin Add (TNat 2) (TList [TNat 3,TAscr (TNat 4) TyZ] Nothing)

* ``:desugar`` shows the desugared core language term corresponding to
  an expression.

    ::

        Disco> :desugar [3,4]
        CCons 1 [CNum Fraction (3 % 1),CCons 1 [CNum Fraction (4 % 1),CCons 0 []]]

* ``:pretty`` shows the pretty-printed form of a term (without
  typechecking it).

    ::

        Disco> :pretty 2 + [3,4:Int]
        2 + [3, (4 : ℤ)]
