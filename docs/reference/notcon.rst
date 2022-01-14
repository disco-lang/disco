The expression e must have both a blah type and also...
=======================================================

This error occurs sometimes when two incompatible types meet: the
context in which an expression is used requires it to have a certain type,
whereas the expression actually has a different type.

For example, consider the following:

::

   Disco> x : N
   Disco> x = 5
   Disco> x(2)
   Error: the expression
     x
   must have both a function type and also the incompatible type
     ℕ.

In this example, the reason ``x`` must have a function type is because
we applied it to an argument, like ``x(2)``.  The only things which
can be applied to arguments are :doc:`functions <function>`.  On the
other hand, we said that the type of ``x`` is ``N``, whch is not a
function.
