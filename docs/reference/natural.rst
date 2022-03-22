Natural numbers
===============

The type of *natural numbers* is written ``N``, ``ℕ``, ``Nat``, or
``Natural`` (Disco always prints it as ``ℕ``, but you can use any of
these names when writing code).  The natural numbers include the
counting numbers 0, 1, 2, 3, 4, 5, ...

:doc:`Adding <addition>` or :doc:`multiplying
<multiplication>` two natural numbers yields another natural number:

::

   Disco> :type 2 + 3
   5 : ℕ
   Disco> :type 2 * 3
   6 : ℕ

Natural numbers cannot be directly :doc:`subtracted <subtraction>` or
:doc:`divided <division>`.  However, ``N`` is a :doc:`subtype` of all
the other numeric types, so using subtraction or division with natural
numbers will cause them to be automatically converted into a
different type like :doc:`integers <integer>` or :doc:`rationals
<rational>`:

::

   Disco> :type 2 - 3
   2 - 3 : ℤ
   Disco> :type 2 / 3
   2 / 3 : 𝔽

Note that some mathematicians use the phrase "natural numbers" to mean
the set of positive numbers 1, 2, 3, ..., that is, they do not include
zero.  However, in the context of computer science, "natural numbers"
almost always includes zero.
