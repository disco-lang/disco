Number of arguments does not match
==================================

When defining a function, there are two ways Disco can figure out how
many arguments it takes: by looking at its declared type, and by
looking at the number of arguments in its definition.  This error
results when these are not the same.

For example, the following definition would yield this error:

::

   f : N -> N -> N
   f x y z = 3

The declared type of ``f``, namely ``N -> N -> N``, says that it takes
two natural number inputs.  However, the definition ``f x y z = ...``
makes it look like it takes three inputs: ``x``, ``y``, and ``z``.

This is not a terribly informative error message, and it will likely
be improved and/or split out into several separate error messages soon.
