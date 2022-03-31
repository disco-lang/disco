Wildcard patterns
=================

A wildcard :doc:`pattern <pattern>` is just an underscore character,
indicating that we do not care about a particular input.  Just like a
:doc:`variable pattern <var-pattern>`, it successfully matches any
input; unlike a variable pattern, it does not define a new name.

For example,

::

   f(_) = 10

defines the function which always returns ``10``, no matter what input
it is given.  This could also be written

::

   f(n) = 10

but since ``n`` is not used, we can explicitly indicate that we do not
care about it by replacing it with a wildcard pattern ``_``.
