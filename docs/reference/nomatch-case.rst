Value did not match any of the branches in a case expression
============================================================

This means that none of the conditions in a :doc:`case expression
<case>` were true.  For example, consider this case expression:

::

   {? 'A'  if  n < 5,
      'B'  if  n > 5
   ?}

When ``n == 5`` specifically, both conditions will be false,
and this error will be generated.

The reason this is an error is that every :doc:`expression
<expression>` must have a value; if all the conditions are false, we
do not know what value the whole case expression should have.

This error may also occur when defining a :doc:`function <function>`
via :doc:`pattern matching <pattern>`, if none of the patterns match
a particular input.  For example, consider the below definition of ``f``:

::

   f : N -> N
   f(3) = 99
   f(2n) = n

If we call this function on an odd input besides 3, it will generate
an error, since neither of the patterns matches:

::

   Disco> f(5)
   Error: value did not match any of the branches in a case expression.

The reason the same error is generated is that internally, function
definitions by cases are translated into case expressions.  For
example, the above definition for ``f`` is translated into something
like

::

   f : N -> N
   f(m) = {? 99 if m is 3, n if m is 2n ?}
