Variables and types
===================

Every :doc:`expression </reference/expression>` in disco has a
:doc:`type </reference/types>`. A *type* is a collection of values
that can all be created and used in the same ways.  For example, the
type of *natural numbers* is the collection of nonnegative integer
values 0, 1, 2, ... which can all be added and multiplied; the type of
*strings* is the collection of text values consisting of a sequence of
characters.

We can ask Disco for the type of an expression with the ``:type``
command, as in the examples below.  Don't worry about what these types
mean for now; the important point is simply that we can always use
``:type`` to ask Disco for the type of anything.

::

   Disco> :type 3
   3 : ℕ
   Disco> :type "hello"
   "hello" : List(Char)
   Disco> :type +
   ~+~ : ℕ × ℕ → ℕ

Types have several benefits:

* The type of an expression is a *guarantee* that when the expression
  is evaluated, it will result in a value of that type.
* Types help make sure that we don't do nonsensical things, like ask
  for the length of a number, or add a number and a string.

Let's begin by looking at a few standard built-in types.

Natural numbers
---------------

The type of *natural numbers* includes the counting numbers 0, 1, 2,
3, 4, 5, ... (*including* zero).  Disco displays the type of natural
numbers using the special symbol ``ℕ``; however, it can also be
written ``Natural``, ``Nat``, or just ``N``.

  - Natural numbers can be *added*; adding two natural numbers
    always results in another natural number.

    ::

       Disco> 1 + 1
       2
       Disco> :type 1 + 1
       1 + 1 : ℕ

  - Natural numbers can also be *multiplied*; again, multiplying two
    natural numbers always results in another natural number.

    ::

       Disco> 3 * 5
       15
       Disco> :type 3 * 5
       3 * 5 : ℕ

Integers
--------

Although we can add and multiply natural numbers, subtracting two
natural numbers will not necessarily yield another natural number.
The type of numbers that support addition, multiplication, and
subtraction is called the *integers*, and includes values like ...,
-3, -2, -1, 0, 1, 2, 3, ...  Disco displays the type of integers using
the special symbol ``ℤ``, but it can also be written ``Z``, ``Int``,
or ``Integer``.

  - We can add, multiply, or subtract two integers, resulting in
    another integer:

    ::

       Disco> :type (-2) + 3
       -2 + 3 : ℤ
       Disco> :type (-2) * 3
       -2 * 3 : ℤ
       Disco> :type (-2) - 3
       -2 - 3 : ℤ

  - Even if we subtract two natural numbers, we get an integer:

    ::

       Disco> 1 - 3
       -2
       Disco> :type 1 - 3
       1 - 3 : ℤ

  - Even if we subtract two natural numbers which result in a positive
    number, Disco still says the result is an integer.  This is
    because a type is supposed to be a *guarantee* that Disco can make
    about the outcome of a program, *without* running it first.
    Without running the program, Disco cannot guarantee that every use
    of subtraction will result in a positive number, so it must be
    conservative and say that anything using subtraction results in an
    integer.

    ::

       Disco> 5 - 2
       3
       Disco> :type 5 - 2
       5 - 2 : ℤ
