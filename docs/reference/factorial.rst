Factorial
=========

*Factorial*, written ``!``, is a :doc:`unary operator <operator>`
written after its argument, defined as the product of all the natural
numbers from 1 up to the :math:`n`, that isx, :math:`n! = 1 \times 2
\times 3 \times \dots \times n`.

::

   Disco> :doc !
   ~! : ℕ → ℕ

   n! computes the factorial of n, that is, 1 * 2 * ... * n.

   https://disco-lang.readthedocs.io/en/latest/reference/factorial.html

   Disco> 3!
   6
   Disco> 4!
   24
   Disco> 4! == 1 * 2 * 3 * 4
   true
   Disco> (4!)!
   620448401733239439360000
   Disco> ((4!)!)!
   Error: that number would not even fit in the universe!
   Disco> 0!
   1

Note that :math:`0! = 1` by definition, since a product of zero things
should be the identity value for multiplication.
