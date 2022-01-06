Expressions
===========

An *expression* is some combination of values, operators, and
functions which describes a *value* (turning an expression into a
value is called *evaluation*).

Examples of expressions in Disco include:

* ``5`` (a single number, string, boolean, variable, etc. by itself is an expression)
* ``1 + 2`` (two or more expressions combined by operators is again an
  expression)

* ``f(x,3) * g(2)`` (function calls are expressions)

Examples of things which are *not* expressions include:

* ``x : N`` (this is a type signature, not an expression; it does not
  have a value, it says what the type of ``x`` is)
* ``x = 3`` (this is a definition)

Be careful not to confuse ``x = 3`` (a definition of the
:doc:`variable` ``x``) with ``x == 3`` (an expression which has a
value of either ``true`` or ``false`` depending on whether ``x``
is equal to ``3`` or not).
