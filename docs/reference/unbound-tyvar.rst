Unknown type variable
=====================

This error always refers to a :doc:`type definition <tydef>`, which
uses a type variable that was not a parameter of the type being
defined.  For example:

::

   Disco> type T(a,b) = N * c
   Error: Unknown type variable 'c'.

In this example, we are defining the type ``T`` which has parameters
``a`` and ``b``.  We are thus allowed to use ``a`` and ``b`` anywhere
inside the definition of ``T``.  However, here we use ``c``, which is
not defined.

- Did you misspell a variable name?

- Did you forget to add the variable as a parameter of the type?  For
  example, if we want to define a type of polymorphic trees, but write
  ``type T = Unit + a * T * T``, we would get this error; what we
  should write insitead is ``type T(a) = Unit + a * T(a) * T(a)``.
