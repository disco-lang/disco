string
======

The Disco standard :doc:`string <string>` library can be :doc:`imported <import>` with

::

   import string

Currently, it does not have much:

* It defines a :doc:`type synonym <typedef>` ``String`` to be equal to
  ``List(Char)``.

* It defines a function ``unlines : List(String) -> String`` which
  turns a list of strings into a single string joined with newline
  characters.

More will probably be added in the future.
