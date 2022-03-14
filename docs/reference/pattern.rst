Pattern matching
================

:doc:`Functions <function>` in Disco can be defined using *pattern
matching*, which in general looks like this:

::

   f(pattern) = expression

This means, roughly, "if the input to the function ``f`` looks like
``pattern``, then the output of ``f`` on that input should be
``expression``."  For example, ``f(5) = 29`` means that if the input
to ``f`` is the number ``5``, then the output should be ``29``.

Functions can be defined by multiple pattern-match *clauses*; Disco
tries the clauses in order, one by one, and picks the first one that
matches.  (Note that :doc:`case expressions <case>` can also be used
to define functions, in case more sophisticated logic is needed.)  For
example,

::

   f(2) = 12
   f(2k) = k+1
   f(n) = 2n+1

means, "First, if the input to ``f`` is specifically ``2``, then
return ``12``; next, if the input to ``f`` is even (*i.e.* of the
form ``2k`` for some integer ``k``), return ``k+1``; otherwise, for
any other input, which we will call ``n``, return ``2n+1``."

The above example uses a :doc:`literal pattern <lit-pattern>`, an
:doc:`arithmetic pattern <arith-pattern>`, and a :doc:`variable
pattern <var-pattern>`; see the links below for more specific
information about the different types of patterns that can be used.

.. toctree::

  - var-pattern
  - lit-pattern
  - wild-pattern
  - arith-pattern
  - prod-pattern
  - sum-pattern
