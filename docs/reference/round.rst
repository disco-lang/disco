Rounding
========

Sometimes, when we have a :doc:`fractional <fraction>` or
:doc:`rational <rational>` number, we want to get rid of the
fractional part and turn it into an :doc:`integer <integer>` or
:doc:`natural number <natural>`.  This can be done with the ``floor``
and ``ceiling`` operators.

* ``floor x``, also written ``⌊ x ⌋``, returns the largest integer
  which is less than or equal to ``x``.  For example:

    ::

       Disco> floor(1/2)
       0
       Disco> floor(7/2)
       3
       Disco> floor(3)
       3
       Disco> floor(-1/2)
       -1

    Notice that ``floor`` always rounds *down*, even for negative
    numbers. (This is how mathematicians think about ``floor``, and is
    the most mathematically elegant definition; however, note that in
    some other programming languages, ``floor`` always rounds *towards
    zero* instead, so *e.g.* ``floor(-1/2)`` would be ``0``.)

* Likewise, ``ceiling x``, also written ``⌈ x ⌉``, returns the
  smallest integer which is greater than or equal to ``x``.  For example:

    ::

       Disco> ceiling(1/2)
       1
       Disco> ceiling(7/2)
       4
       Disco> ceiling(3)
       3
       Disco> ceiling(-1/2)
       0

