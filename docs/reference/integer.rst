Integers
========

The type of *integers* is written ``Z``, ``ℤ``, ``Int``, or
``Integer``.  The integers include the positive and negative
counting numbers (as well as zero): ..., -3, -2, -1, 0, 1, 2, 3, ...

:doc:`Adding <addition>`, :doc:`multiplying <multiplication>`, or
:doc:`subtracting <subtraction>` two integers yields another
integer. Trying to :doc:`divide <division>` two integers will
automatically convert the result to a :doc:`rational number
<rational>`:

::

   Disco> :type 2 * (-3)
   2 * (-3) : ℤ
   Disco> :type 2 / (-3)
   2 / (-3) : ℚ
