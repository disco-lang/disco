Case expressions
================

We can use "case expressions" to choose among multiple alternatives.

Basic case expressions with conditions
--------------------------------------

The basic form of a case expression is as follows:

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

If *none* of the conditions in a case expression are true, it is an
error: see :doc:`nomatch-case`.

Case expressions with conditions and patterns
---------------------------------------------

More generally, case expressions can use :doc:`pattern matching
<pattern>` in addition to Boolean conditions, and each alternative in
a case expression can have multiple conditions.  The most general form of a
case expression is as follows:

::

   {? alternative1   guard11 guard12 ...,
      alternative2   guard21 guard22 ...,
      ...
   ?}

where each ``guard`` has one of two forms:

- ``if <condition>``.  This guard succeeds if the condition is true.
- ``if <expression> is <pattern>``.  This guard succeeds if the given
  :doc:`expression <expression>` matches the :doc:`pattern <pattern>`;
  furthermore, any :doc:`variables <var-pattern>` in the pattern will be
  defined locally within the corresponding ``alternative`` as well as
  any subsequent guards in the same clause.

The keyword ``when`` can also be used as a synonym for ``if``.
