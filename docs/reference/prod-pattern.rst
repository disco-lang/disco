Tuple patterns
==============

A tuple :doc:`pattern <pattern>` consists of a :doc:`tuple expression
<product-type>` (*i.e.* a pair, triple, ...) used as a pattern.  Each
component of the tuple can itself be any pattern.  The simplest kind
of tuple pattern would be a pair with :doc:`variables <var-pattern>`,
like

::

   f : N * N -> Q
   f(x,y) = ...

This is extremely common when defining functions that take multiple
inputs.  Functions that take more than two inputs can also be defined
similarly:

::

   f : N * Z * Z -> Z
   f(a,b,c) = ...

(See :doc:`the page on pair types <product-type>` for more details on
how n-tuples work.)

The components of a pair pattern can themselves be any pattern,
however, not just variables.  For example,

::

   f : N * N -> N
   f(2n+1, 3) = 17
   f(x, y) = x + y

The above example defines ``f`` to yield ``17`` when applied to any
tuple consisting of an odd number paired with ``3`` (using an
:doc:`arithmetic pattern <arith-pattern>` and a :doc:`literal pattern
<lit-pattern>`), and ``x + y`` when applied to any other pair ``(x,y)``.
