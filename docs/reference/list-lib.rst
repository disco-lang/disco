list
====

The Disco standard :doc:`list <list>` library can be :doc:`imported <import>` with

::

   import list

It provides the following functions.  For more information, import the
``list`` module and ask for documentation on any function.  For
example,

::

   :doc foldr

You can also look at the `Disco source code of the list library`_.

.. _`Disco source code of the list library`: https://github.com/disco-lang/disco/blob/main/lib/list.disco

* ``foldr`` is for *folding* a list, *i.e.* reducing a list to a
  summary value.

* ``append`` glues two lists end-to-end into a single list.

* ``concat`` flattens a list of lists into a single list.

* ``eachlist`` applies a function to every element of a list.  This
  can also be done via the built-in ``each`` function, but
  ``eachlist`` is provided so you can look at its implementation.

* ``take`` returns a prefix of a given length.

* ``length`` computes the length of a list.

* ``listCP`` computes the Cartesian product of two lists.

* ``zipWith`` applies a binary function pairwise to the elements of
  two lists.

* ``filterlist`` filters a list according to a predicate.  Again, this
  can also be done with the built-in ``filter`` function, but it's
  nice to see how ``filterlist`` is implemented.
