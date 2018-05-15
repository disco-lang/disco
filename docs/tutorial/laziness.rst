
********
Laziness
********

Disco takes a hybrid approach to laziness.

* To facilitate compositionality and allow for things such as infinite
  lists, structured types (lists, pairs, etc.) in disco are lazy.
* To avoid surprising performance issues, numeric types in disco are
  strict.

Examples coming soon.


Let expressions
===============

*Let expressions* are a mechanism for defining new variables for local
use within an expression.  For example, ``3 + (let y = 2 in y + y)``
evaluates to ``7``: the expression ``y + y`` is evaluated in a context
where ``y`` is defined to be ``2``, and the result is then added to
``3``.  The simplest syntax for a let expression, as in this example,
is ``let <variable> = <expression1> in <expression2>``.  The value of
the let expression is the value of ``<expression2>``, which may
contain occurrences of the ``<variable>``; any such occurrences will
take on the value of ``<expression1>``.

More generally:

* A ``let`` may have multiple variables defined before ``in``,
  separated by commas;
* Each variable may optionally have a type annotation.
* The definitions of later variables may refer to previously defined
  variables.
* However, the definition of a variable in a ``let`` may not refer to
  itself; only top-level definitions may be recursive.

Here is a (somewhat contrived) example which demonstrates all these
features:

.. literalinclude:: example/let.disco
   :language: idris
   :caption:

An important thing to note is that a given definition in a ``let``
expression will only ever be evaluated (at most) once, even if the
variable is used multiple times.  ``let`` expressions are thus a way
for the programmer to ensure that the result of some computation is
shared. ``let x = e in f x x`` and ``f e e`` will always yield the
same result, but the former might be more efficient, if ``e`` is
expensive to calculate.
