Definition versus equality testing
==================================

In standard mathematical notation, the ``=`` symbol can be used in
at least two distinct (yet related) contexts:

- To *define* things, as in, "Let :math:`x = 3y + 2`, and consider..."
  In this example sentence, :math:`x = 3y + 2` *defines* the
  variable ``x`` as standing for the expression ``3y + 2``.

- As a *relation* which can hold, or not, as in, "If :math:`x = 3y +
  2`, then ... but otherwise ...".  In this example sentence,
  :math:`x` and :math:`y` are already defined, and :math:`x = 3y + 2`
  is something that can be either true or false.

Notice how the expression :math:`x = 3y + 2` means two very different
things depending on the context, and the context is defined entirely
by the English words surrounding the symbols!  Disco does not have the
luxury of using English words to express what we mean; instead, Disco
must use two different symbols instead.  The ``=`` symbol is used to
express :doc:`definitions <definition>`, as in the first example;
whereas the ``==`` symbol :doc:`tests whether two things are equal
<compare>`, as in the second example.
