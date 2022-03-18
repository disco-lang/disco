Addition
========

[This page concerns the ``+`` operator on numbers; for the ``+``
operator on :doc:`types <types>`, see :doc:`sum types <sumtype>`; for
the ``+`` operator on :doc:`graphs <graphs>`, see :doc:`overlay
<overlay>`.]

Values of all :doc:`numeric types <numeric>` can be added using the ``+``
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

which says that it is a :doc:`function <function>` taking :doc:`two
<product>` natural numbers and returning a natural number.  However,
as mentioned above, it also works on other numeric types such as
integers and rational numbers.

The ``+`` operator can also be used on :doc:`graphs`, where it has the
meaning of :doc:`overlaying <overlay>` two graphs.
