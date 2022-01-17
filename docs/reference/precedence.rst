Operator precedence
===================

When we write something like :math:`1 + 2 \times 3`, how do we know what it
means?  Does it mean :math:`(1 + 2) \times 3`, or :math:`1 + (2 \times 3)`?  Of course,
you are familiar with the usual "order of operations", where
multiplication comes before addition, so in fact :math:`1 + 2 \times 3` should
be interpreted as :math:`1 + (2 \times 3) = 1 + 6 = 7`.

Another way to say this is that multiplication has *higher precedence*
than addition.  If we think of operators as "magnets" that attract
operands, higher precedence operators are like "stronger magnets".

Every operator in Disco has a precedence level, and Disco uses these
levels to determine where to put parentheses in expressions like ``1 +
2 * 3`` or ``5 > 2 ^ 2 + 1 and 7 > 2 ==> true``.  You might have
memorized something like PEMDAS, but Disco has so many operators that
memorizing their precedence levels is out of the question!  Instead,
we can use the ``:doc`` command to show us the precedence level of
different operators.

::

   Disco> :doc ^
   ~^~ : ℕ × ℕ → ℕ
   precedence level 13, right associative

   Disco> :doc +
   ~+~ : ℕ × ℕ → ℕ
   precedence level 7, left associative

   Disco> :doc >
   ~>~ : ℕ × ℕ → Bool
   precedence level 5, right associative

   Disco> :doc and
   ~and~ : Bool × Bool → Bool
   precedence level 4, right associative

   Disco> :doc ==>
   ~==>~ : Bool × Bool → Bool
   precedence level 2, right associative

XXX :doc:`associativity <associativity>`
