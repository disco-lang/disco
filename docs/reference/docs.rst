Documentation
=============

Disco allows special *documentation* :doc:`comments <comment>`, which
begin with three vertical bars (``|||``) at the beginning of a line.
Anything written after ``|||`` is documentation which will be attached
to the next definition (either a :doc:`type definition <typedef>` or a
:doc:`variable <variable>` definition).  This documentation can later
be accessed with the ``:doc`` command.  For example, suppose we have the
following in a file called ``cool.disco``:

.. literalinclude:: example/cool.disco
   :language: idris
   :caption:

Then at the disco prompt we can load the file, and see the
documentation for ``f`` using the ``:doc`` command:

::

   Disco> :load cool.disco
   Loading cool.disco...
   Loaded.
   Disco> :doc f
   f : ℕ → ℕ

   f is a cool function which computes a thing.
   It has two lines of documentation.

