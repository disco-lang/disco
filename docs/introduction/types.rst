Types
=====

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
* Types aid us in keeping things straight in our own heads, especially
  once we start dealing with more complex things such as functions and
  sets.

Let's begin by looking at a few standard built-in types.

Natural numbers
---------------

The type of *natural numbers* encompasses the counting numbers 0, 1,
2, 3, 4, 5, ... (*including* zero).  Disco displays the type of
:doc:`natural numbers </reference/natural>` using the special symbol
``ℕ``; however, it can also be written ``Natural``, ``Nat``, or just
``N``.

  - Natural numbers can be *added*; adding two natural numbers
    always results in another natural number (that is, the natural
    numbers are "closed under :doc:`addition </reference/addition>`").

    ::

       Disco> 1 + 1
       2
       Disco> :type 1 + 1
       1 + 1 : ℕ

  - Natural numbers can also be *multiplied*; again, multiplying two
    natural numbers always results in another natural number (that is,
    the natural numbers are "closed under :doc:`multiplication
    </reference/multiplication>`").

    ::

       Disco> 3 * 5
       15
       Disco> :type 3 * 5
       3 * 5 : ℕ

Integers
--------

Although we can add and multiply natural numbers, subtracting two
natural numbers will not necessarily yield another natural
number---the natural numbers are not closed under :doc:`subtraction
</reference/subtraction>`.  The type of numbers that support addition,
multiplication, and subtraction is called the *integers*, and includes
values like ..., -3, -2, -1, 0, 1, 2, 3, ...  Disco displays the type
of :doc:`integers </reference/integer>` using the special symbol
``ℤ``, but it can also be written ``Z``, ``Int``, or ``Integer``. (The
traditional symbol ``ℤ`` stands for the German word for integers, *Zahlen*.)

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

Subtyping
---------

In some cases, Disco can automatically convert between types if no
information would be lost.  For example, we saw in an example above
that Disco can automatically convert natural numbers to integers,
because every natural number is an integer.  Even though ``5`` and
``2`` individually have type ``N``, if we subtract them Disco converts
them to type ``Z`` in order to be able to do the subtraction:

::

   Disco> :type 5
   5 : ℕ
   Disco> :type 2
   2 : ℕ
   Disco> :type 5 - 2
   5 - 2 : ℤ

On the other hand, not every integer is a natural number (for example,
``-5`` is an integer but not a natural number), so Disco cannot
automatically convert the other way.

This automatic conversion is called :doc:`subtyping
</reference/subtypes>`.  We will discuss it in more depth later; for
now it mostly won't make much difference.

Exercises
---------

What **type** will Disco give to each of the following expressions?
(You do not have to predict their value.)  Make a prediction, then
use the ``:type`` command to see if you were right.

- ``1``
- ``777``
- ``-2``
- ``0``
- ``1 + 99``
- ``(-1) + 99``
- ``1 + (-99)``
- ``19 - 6``

Fractional and Rational numbers
-------------------------------

Just as subtracting two natural numbers may not give us another
natural number, we also cannot *divide* two natural numbers.

- The natural numbers plus fractions such as ``2/3`` make up the type
  of *fractional numbers*, written ``F``, ``𝔽``, ``Frac`` or
  ``Fractional``.  This type supports addition, multiplication, and
  :doc:`division </reference/division>`.

- The integers plus all positive or negative fractions make up the
  type of *rational numbers*, written ``Q``, ``ℚ``, or ``Rational``.
  This type supports all four standard arithmetic operations:
  addition, multiplication, subtraction, and division.

You will learn more about these types and how to convert between them
later; for now it's important just to know that they exist and to
understand the basic distinctions between them.

Note that Disco does not have so-called "floating-point" numbers: all
rational numbers are stored exactly as a fraction, not as a decimal
approximation.  For example,

::

   Disco> (1+1)/(3+4)
   2/7

The result of ``(1+1)/(3+4)`` is simply displayed as the fraction
``2/7``, instead of as a decimal approximation like
``0.2857142857142857``.  However, we can still use decimal notation to
input rational numbers:

::

   Disco> 1.2 + 3.5
   47/10

Exercises
---------

What **type** do you think Disco will give to each of the following
expressions?  (You do not have to predict their value.)  Make a
prediction, then use the ``:type`` command to see if you were right.

- ``2 / 3``
- ``5 / (-6)``
- ``(-5)``
- ``(2 / 3) + (-5)``
