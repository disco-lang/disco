Addition
========

All :doc:`numeric types <numeric>` support addition using the ``+``
operator.  For example:

::

    Disco> 1 + 2
    3
    Disco> (-5) + 2/3
    -13/3

If you ask for the :doc:`type <types>` of ``+``, you get

::

    Disco> :type +
    ~+~ : ℕ × ℕ → ℕ

which says that it is a :doc:`function <functions>` taking :doc:`two
<product>` natural numbers and returning a natural number.  However,
as mentioned above, it also works on other numeric types such as
integers and rational numbers.