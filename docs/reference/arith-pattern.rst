Arithmetic patterns
===================

An arithmetic :doc:`pattern <pattern>` consists of an arithmetic
expression, containing one or more variables, used as a pattern.  For
example,

::

   f(n+2) = ...
   f(2n+7) = ...
   f(p/q) = ...
   f(-p/(q+1)) = ...

are all examples of arithmetic patterns.  A full description of how
arithmetic patterns work would be too complex; put simply, an
arithmetic pattern matches whenever the input is a number of the given
form.  For example, ``f(2n+1) = ...`` matches whenever there is a
number ``n`` such that the input is of the form ``2n+1``.  This is a
common way to define functions depending on whether the input is even
or odd:

::

   f : N -> N
   f(2n) = ... n ...     -- even inputs
   f(2n+1) = ... n ...   -- odd inputs

Note that ``f(p/q)`` matches whenever the input is a rational number
with a numerator of ``p`` and a denominator of ``q``.  In all other
cases, arithmetic patterns are generally required to have only one
variable.  Otherwise, the pattern is ambiguous.  For example, ``f(a+b)
= ...`` is not allowed, since ``a + b = n`` has many solutions for a
given ``n``; there is no way to determine what ``a`` and ``b`` must
be.

Arithmetic patterns can sometimes allow us to define things in an
alternative way that does not require :doc:`subtraction
<subtraction>`.  For example, one way to define the :doc:`factorial
<factorial>` function is as follows:

::

   fact : N -> N
   fact(0)   = 1
   fact(n+1) = (n+1) * fact(n)
