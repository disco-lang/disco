
**************
Disco Tutorial
**************

Disco is a small yet expressive, pure functional programming language
designed especially to be used in the context of a discrete
mathematics course. XXX more description of big ideas and intended
uses.

After building disco with ``stack build``, to run the disco REPL
(Read-Eval-Print Loop) type ``stack exec disco`` at a command prompt.
You should see a disco prompt that looks like this:

::

    Disco>

Arithmetic expressions
======================

To start out, you can use disco as a simple calculator.  For
example, try entering the following expressions, or others like them,
at the ``Disco>`` prompt:

* ``2 + 5``
* ``5 - 8``
* ``5 * (-2)``
* ``(1 + 2)(3 + 4)``
* ``2 ^ 5``
* ``3/7 + 2/5``
* ``2 ^ (-5)``

Notice that it is not always necessary to write ``*`` for
multiplication: as is standard mathematical notation, we may often
omit it, as in ``(1 + 2)(3 + 4)``, which means the same as ``(1 + 2) *
(3 + 4)``. (For precise details on when the asterisk may be omitted,
see XXX.)  The last two expressions may also come as a bit of a
surprise if you are already used to other languages such as Java or
Python, which would yield a floating-point (*i.e.* real) number.  In
fact, being a language for *discrete* mathematics, disco does not have
real numbers at all, only integers and rational numbers.  However,
rational numbers can still be entered using decimal notation.  Try
these expressions as well:

* ``2.3 + 1.6``
* ``1/5.``
* ``1/7.``

Disco automatically picks either fractional or decimal notation for
the output, depending on whether any values with decimal points were
used in the input (for example, compare ``1/5`` and ``1/5.``, or
``1./5``).  Note that ``1/7.`` results in ``0.[142857]``;
can you figure out what the brackets indicate?

The standard ``floor`` and ``ceiling`` operations are built-in:

::

    Disco> floor (17/3)
    5
    Disco> ceiling (17/3)
    6

* integer division
* mod
* abs

XXX some more text

* divides
* choose
* factorial
* sqrt
* lg

Types
=====

N, Z, QP, Q
