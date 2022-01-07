Operators
=========

An *operator* is a function that is written in a special way.  Normal
functions are written before their arguments, like ``f(x,y)``.
*Binary* (two-argument) operators are symbols or words which are written *in between*
their two arguments, like ``1 + 2``.  Disco has many built-in
operators which are symbols (like ``+``, ``*``, ``/``, *etc.*) as well
as a few which are words (like ``mod``, ``choose``, and ``divides``).

Disco also has three *unary* operators: arithmetic negation (``-``)
and logical negation (``¬`` or ``not``) are written in front of their
one argument, and factorial (``!``) is written after its argument.

Twiddle notation
----------------

Disco has a special syntax for talking about operators on their own,
without any arguments: a tilde (or "twiddle") (``~``) goes in each
place where an argument would be.  For example, to talk about the
``+`` operator on its own we can write ``~+~``.  To talk about the
factorial operator we would write ``~!``, because factorial only takes
a single argument which goes before it.  Disco will use this "twiddle
notation" when you ask it for the type of an operator:

::

   Disco> :type !
   ~! : ℕ → ℕ
   Disco> :type ~!
   ~! : ℕ → ℕ

Note that in this case, we can write ``!`` or ``~!`` and Disco
understands either one.

The twiddle notation is also useful when giving an operator as an
argument to a :doc:`higher-order function <hof>`:

::

   Disco> foldr(~+~, 0, [1 .. 10])
   55

Operator documentation
----------------------

You can ask for :doc:`documentation <docs>` about operators directly,
for example:

::

   Disco> :doc !
   ~! : ℕ → ℕ
   XXX UPDATE ME!
