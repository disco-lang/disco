Ellipsis
========

:doc:`Sets <set>`, :doc:`lists <list>`, and :doc:`bags <bag>` may be defined using *ellipsis*,
that is, two or more dots meaning (intuitively) "and so on".  For
example:

::

   Disco> {1 .. 5}
   {1, 2, 3, 4, 5}
   Disco> ⟅1 .. 5⟆
   ⟅1, 2, 3, 4, 5⟆
   Disco> [2, 4 ... 10]
   [2, 4, 6, 8, 10]
   Disco> [1, 4, 9 ... 100]
   [1, 4, 9, 16, 25, 36, 49]

Note, Disco isn't actually being all that smart here, and it won't
work for any pattern at all.  For example, it fails miserably to
understand that we want a list of primes:

::

   Disco> [2, 3, 5, 7, 11 ... 100]
   [2, 3, 5, 7, 11, 22, 48, 100]

So what is Disco actually doing?  Note first that there must always be
a single number after the dots.

- If there is a single number before the dots:

  - If the first number is smaller than the last number, the resulting
    list or set starts at the first number and counts up by ones until
    reaching the last number.

      ::

         Disco> {1 .. 5}
         {1, 2, 3, 4, 5}

  - If the first number is greater than the last number, it counts
    down instead of up.

      ::

         Disco> [10 .. 7]
         [10, 9, 8, 7]

- If there are :math:`k > 1` numbers before the dots, Disco fits a
  :math:`k-1`-degree polynomial to the numbers and then extends it
  until the next value would be greater than the value after the dots.

    - For example, for :math:`k = 2`, this just means that Disco will
      extend the list using a constant gap between consecutive values
      (the same as the gap between the first two numbers):

        ::

           Disco> [1, 3 .. 10]  -- counting by twos
           [1, 3, 5, 7, 9]
           Disco> {5, 10 .. 40} -- counting by fives
           {5, 10, 15, 20, 25, 30, 35, 40}

    - For :math:`k = 3`, Disco will use a quadratic polynomial, which
      means we can generate things like the squares or the triangular
      numbers:

        ::

           Disco> [1, 4, 9 ... 100]
           [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
           Disco> [1, 3, 6 ... 28]
           [1, 3, 6, 10, 15, 21, 28]
