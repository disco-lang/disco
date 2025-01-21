Numeric types
=============

.. toctree::

   natural
   integer
   fraction
   rational

Disco has four :doc:`types <types>` which represent numbers:

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

.. image:: ../images/diamond.png
   :width: 300
   :alt: Diamond lattice
   :align: center

Each type is a subset, or :doc:`subtypes`, of the type or types above
it.  For example, the fact that :math:`\mathbb{N}` is below
:math:`\mathbb{Z}` means that every natural number is also an integer.

* The values of every numeric type can be :doc:`added <addition>`
  and :doc:`multiplied <multiplication>`.
* The arrow labelled :math:`x-y` indicates that going up and to the
  left in the diamond (*i.e.* from :math:`\mathbb{N}` to Z or F to Q)
  corresponds to adding the ability to do subtraction. That is, values
  of types on the upper left of the diamond (:math:`\mathbb{Z}` and
  :math:`\mathbb{Q}`) can also be :doc:`subtracted <subtraction>`.
* Going up and to the right corresponds to adding the ability to do
  division; that is, values of the types on the upper right of the
  diamond (:math:`\mathbb{F}` and :math:`\mathbb{Q}`) can also be
  :doc:`divided <division>`.
* To move down and to the right (*i.e.* from :math:`\mathbb{Z}` to :math:`\mathbb{N}`, or from
  :math:`\mathbb{Q}` to :math:`\mathbb{F}`), you can use :doc:`absolute value <abs>`.
* To move down and to the left (*i.e.* from :math:`\mathbb{F}` to :math:`\mathbb{N}`, or from
  :math:`\mathbb{Q}` to :math:`\mathbb{Z}`), you can take the :doc:`floor or ceiling <round>`.
