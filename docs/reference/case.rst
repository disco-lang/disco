Case expressions
================

We can use conditions to choose between alternatives
using a "case expression", which looks like the following:

::

   {? alternative1   if  condition1,
      alternative2   if  condition2,
      ...
      alternativeN   otherwise
   ?}

This will try each condition, starting with the first,
until finding the first condition that is true.  Then the value of the
entire case expression will be equal to the corresponding alternative.
The ``otherwise`` case will always be chosen if it is reached.
Each condition must be an expression of type :doc:`Bool <bool>`; the
alternatives can have any type (though they must all have the *same*
type, whatever it is).

For example, consider the definition of the ``caseExample`` function
below:

::

   caseExample : N -> N
   caseExample(n) =
     {? n + 2    if n < 10 \/ n > 20,
        0        if n == 13,
        77n^3    if n == 23,
        n^2      otherwise
     ?}

Here are a few sample inputs and outputs for ``caseExample``, with an
explanation of each:

- ``caseExample(5) == 7``: the first condition is true (since ``5 < 10``),
  so the result is ``5 + 2``.

- ``caseExample(23) == 25``: the first condition is again true (since
  ``23 > 20``), so the result is ``23 + 2``. Note that the first true
  condition is always chosen, so it does not matter that the later
  condition ``n == 23`` would also be true.

- ``caseExample(13) == 0``: the first condition is false (13 is neither
  ``< 10`` nor ``> 20``, but the second condition (``13 == 13``) is true.

- ``caseExample(12) == 144``: the first three conditions are all false,
  so the ``otherwise`` case is used, with the result ``12^2``.
