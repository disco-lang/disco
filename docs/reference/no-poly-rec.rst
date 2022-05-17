Recursive occurrences of T may only have type variables as arguments
====================================================================

For technical reasons, when :doc:`defining a parameterized type
<typedef>`, any recursive occurrences of the type in its own definition
can only have type variables as arguments.  For example, this is
perfectly OK:

::

   type T(a) = Unit + a * T(a) * T(a)

Notice how every occurrence of ``T`` on the right-hand side of the
``=`` has the variable ``a`` as an argument.

Even this is OK:

::

   type Alt(a,b) = Unit + a * Alt(b,a)

In this example, the ``Alt`` on the right-hand side of the ``=`` has
its arguments in the opposite order from the one on the left-hand
side, but that is OK as long as they are all type variables.

These examples, on the other hand, are not OK:

::

   type Bad1(a) = Unit + Bad1(N)
   type Bad2(a) = Unit + Bad2(Bad2(a))
