Definition versus equality testing
==================================

In standard mathematical notation, the ``=`` symbol can be used in
at least two distinct (yet related) contexts:

- To *define* things, as in, "Let :math:`x = 3y + 2`, and consider..."
  In this example sentence, :math:`x = 3y + 2` *defines* the
  variable ``x`` as standing for the expression ``3y + 2``.

- As a *relation* which can hold, or not, as in, "If :math:`x = 3y +
  2`, then ... but otherwise ...".  In this example sentence,
  :math:`x` and :math:`y` must already be defined, and :math:`x = 3y + 2`
  is something that is either true or false.

Notice how the exact same expression :math:`x = 3y + 2` is used in
both examples, but means two very different things depending on the
context---which is defined entirely by the English words surrounding
the symbols!  Disco does not have the luxury of using English words to
figure out what we mean; instead, Disco must use two different
symbols.  The ``=`` symbol is used to express :doc:`definitions
<definition>`, as in the first example; whereas the ``==`` symbol
:doc:`tests whether two things are equal <compare>`, as in the second
example.
