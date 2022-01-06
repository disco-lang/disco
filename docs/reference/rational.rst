Rational numbers
================

The type of *rational numbers* is written ``Q``, ``ℚ``, or
``Rational``.  The rational numbers can be thought of as the
combination of ``𝔽`` and ``ℤ``, and include zero, positive and
negative counting numbers, and positive and negative fractions.

::

   Disco> :type -3/5
   -3 / 5 : ℚ

Doing any of the four :doc:`arithmetic <arithmetic>` operations
(:doc:`addition <addition>`, :doc:`multiplication <multiplication>`,
:doc:`subtraction <subtraction>` or
:doc:`division <division>`) on two rational numbers results in another rational number.

Note that Disco does not have any way to work with *irrational*
numbers such as :math:`\pi` or :math:`\sqrt{2}`; such numbers are
typically not needed in discrete mathematics, and including them would
make the language much more complicated.
