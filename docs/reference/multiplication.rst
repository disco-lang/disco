Multiplication
==============

[This page concerns the ``*`` operator on numbers; for the ``*``
operator on :doc:`types <types>`, see :doc:`product types
<product-type>`; for the ``*`` operator on :doc:`graphs <graphs>`, see
:doc:`connect <connect>`.]

All :doc:`numeric types <numeric>` can be multiplied using the ``*``
operator.  For example:

::

    Disco> 2 * 3
    6
    Disco> (-5) * 2/3
    -10/3

In some cases, the ``*`` symbol is not needed: putting two things next
to each other means multiplying them.  For example:

::

   Disco> (1 + 2)(3 + 4)
   21
   Disco> x : N
   Disco> x = 5
   Disco> 3x
   15

The exception is that putting a :doc:`variable <variable>` directly on the left
of something else is interpreted as :doc:`function application
<function>`.

The ``*`` operator can also be used on :doc:`graphs`, where it has the
meaning of :doc:`connecting <connect>` two graphs.
