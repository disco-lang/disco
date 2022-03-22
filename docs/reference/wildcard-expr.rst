Wildcards are not allowed in expressions
========================================

A :doc:`wildcard pattern <wildcard>`, written using an underscore
(``_``), can be used in a pattern (on the *left* side of an equals
sign) to indicate that you don't care about a certain value.

However, you are not allowed to use a wildcard anywhere on the
*right* side of an equals sign.  If you promise to give me two books
you can't just give me one and say you don't care about the other one;
likewise, if you promise to deliver (say) a pair of natural numbers,
you not allowed to say you don't care what one of them is:

::

   f : Char -> N * N
   f(_) = (_, 3)

The first ``_`` is fine: the function ``f`` doesn't need to care what
``Char`` input it is given.  But the second ``_`` is not OK: ``f`` has
promised to return a pair of natural numbers, and it had better
fulfill its promise.
