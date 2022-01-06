Fractional numbers
==================

The type of *fractional numbers* is written ``F``, ``𝔽``, ``Frac``, or
``Fractional``.  The fractional numbers include all the natural
numbers (0, 1, 2, ...) along with all the positive fractions formed
from the ratio of two natural numbers (such as 1/2, 13/7, 56/57, ...)

:doc:`Adding <addition>`, :doc:`multiplying <multiplication>`, or
:doc:`dividing <division>` two fractional numbers yields another
fractional number. Trying to :doc:`subtract <subtraction>` two
fractional numbers will automatically convert the result to a
:doc:`rational number <rational>`:

::

   Disco> :type (1/2) * (2/3)
   1 / 2 * 2 / 3 : 𝔽
   Disco> :type 1/2 - 2/3
   1 / 2 - 2 / 3 : ℚ

The special sets ℕ (natural numbers), ℤ (integers), and ℚ (rational
numbers) are very common in mathematics and computer science, but the
set of fractional numbers 𝔽 is not common at all (in fact, I made up
the name and the notation).  People usually start with the natural
numbers ℕ, extend them with subtraction to get the integers ℤ, and
then extend those again with division to get the rational numbers ℚ.
However, there is no reason at all that we can't do it in the other
order: first extend the natural numbers ℕ with division to get the
fractional numbers 𝔽, then extend with subtraction to get ℚ.  Having
all four types in Disco (even though one of them is not very common in
mathematical practice) makes many things simpler and more elegant.

