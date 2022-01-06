Numeric types
=============

.. toctree::

   natural
   integer
   fraction
   rational

Disco has four :doc:`types <type>` which represent numbers:

* :doc:`Natural numbers <natural>`, written ``N``, ``ℕ``, ``Nat``, or
  ``Natural``.  These represent the counting numbers 0, 1, 2,
  ... which can be added and multiplied.

    ::

       Disco> :type 5
       5 : ℕ

* :doc:`Integers <integer>`, written ``Z``, ``ℤ``, ``Int``, or
  ``Integer``, allow negative numbers such as ``-5``.  They extend the
  natural numbers with subtraction.

    ::

       Disco> :type -5
       -5 : ℤ

* :doc:`Fractional numbers <fraction>`, written ``F``, ``𝔽``,
  ``Frac``, or ``Fractional``, allow fractions like ``2/3``.  They
  extend the natural numbers with division.

    ::

       Disco> :type 2/3
       2 / 3 : 𝔽

* :doc:`Rational numbers <rational>`, written ``Q``, ``ℚ``, or
  ``Rational``, allow both negative and fractional numbers, such as
  ``-2/3``.

    ::

       Disco> :type -2/3
       -2 / 3 : ℚ

We can arrange the four numeric types in a diamond shape, like this:

::

      Q
     / \
    Z   F
     \ /
      N

Each type is a subset, or :doc:`subtype`, of the type or types above
it.  For example, the fact that N is below Z means that every natural
number is also an integer.

* The values of every numeric type can be :doc:`added <addition>`
  and :doc:`multiplied <multiplication>`.
* The types on the upper left of the diamond (``Z`` and ``Q``) can
  also be :doc:`subtracted <subtraction>`.
* The types on the upper right of the diamond (``F`` and ``Q``) can
  also be :doc:`divided <division>`.
* To move down and to the right (*i.e.* from ``Z`` to ``N``, or from
  ``Q`` to ``F``), you can use :doc:`absolute value <abs>`.
* To move down and to the left (*i.e.* from ``F`` to ``N``, or from
  ``Q`` to ``Z``), you can take the :doc:`floor or ceiling <round>`.
