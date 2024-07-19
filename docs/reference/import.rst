Importing other modules
=======================

You can *import* one ``.disco`` module into another, so that the
definitions from the first module are available in the second.  Most
commonly, you can do this to import one or more standard library
modules into your code.  For example, if you are working on a
``.disco`` file and you want to be able to find prime factorizations
of numbers, you can import the ``num`` standard library and then use
its ``factor`` function.

Importing a module is done by writing, *e.g.*

::

   import num

at the *top* of your ``.disco`` file (or at the :doc:`REPL <REPL>`).  Replace
``num`` with the name of whatever file you want to import (without the
``.disco`` suffix).  For example, to import ``myfunctions.disco`` you
could write

::

   import myfunctions
