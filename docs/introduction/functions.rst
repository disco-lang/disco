Functions
=========

*Functions* are central to Disco.  We've already seen a few special
built-in functions: ``floor``, ``abs``, and even the arithmetic
operations like addition, which are really functions with special
notation.  In this section, you'll learn more about functions, their
types, and how to define your own.

Functions as machines
---------------------

What is a *function*?  As a concrete metaphor, think of a function as
a box, with some kind of machinery inside and a slot on each end.  If
you put some kind of *input* into the slot on one end, the machinery
whirrs away and an *output* spits out of the slot on the other end.

More formally, a function is some kind of *rule* or *procedure* that
turns each *input* value (taken from some collection of possible
inputs, called the *domain*) into an associated *output* value (from a
collection of possible outputs, called the *codomain*).  For example,
:math:`\mathit{double}(n) = 2n` defines a function called
:math:`\mathit{double}` which associates each input value :math:`n`
with the output value :math:`2n`.

Function types and definitions
------------------------------

By now you know that :doc:`everything in Disco has a type <types>`, so
functions must have types as well.  The :doc:`type of a function
</reference/function-types>` which takes values of type ``A`` as input
and produces values of type ``B`` as output is written ``A -> B``.
For example, the type of a function which takes natural numbers as
input and produces rational numbers as output would be written ``N ->
Q`` (or ``Natural -> Rational``, or ``ℕ → ℚ``, *etc.*).

Let's see how to define the :math:`\mathit{double}` function in Disco.
It follows the :doc:`same pattern we saw before for defining variables
<variables>`: we first declare the type of the function by writing its
name, a colon, and its type, then we give a definition of the function
on another line.  The only difference is that instead of writing
``double = ...`` we write ``double(n) = ...`` to show that ``double``
is a function taking an input we call ``n``.

::

   double : N -> N
   double(n) = 2n

Note that the name of the input does not matter; there is nothing
magical about naming the input ``n`` (or any other particular name).
If we wanted we could also call the input ``x``, or ``flerb``, or any
other :doc:`valid name <variables>`, and it would still define exactly
the same function:

::

   double : N -> N
   double(flerb) = 2 * flerb

(Here we also used an explicit :ref:`multiplication
<introduction/arithmetic:addition and multiplication>`
operator ``*``, but writing ``2flerb`` or ``2 flerb`` would work too.)

Function application
--------------------

Once a function has been defined, we can use it in any expression by
*applying* it to an input, writing the input in parentheses after the
name of the function.

::

   Disco> 3 + double(5)
   13
   Disco> double(7) + double(2) + double(double(1))
   22

We can also use it as part of the definition of other functions. For
example:

::

   quadruplePlusOne : N -> N
   quadruplePlusOne(n) = double(double(n)) + 1

Typechecking for functions
--------------------------

Disco ensures that every function definition matches its declared
type: that is, if the function is given a value of the declared input
type, then it is guaranteed to produce an output of the declared
output type.

For example, here is a function definition that Disco rejects with a
typechecking error:

::

   bad :: N -> N
   bad(n) = n - 3

The problem is that although the type ``N -> N`` *promises* that it
will always return a natural number as output when given a natural
number as input, this is not true: for example, when ``n`` is 1, ``n -
3`` would be ``-2``, which is not a natural number.

Likewise, Disco ensures that functions are only applied to inputs of
their declared input type.  For example, the ``quadruplePlusOne``
function defined at the end of the previous section is declared to
have type ``N -> N``.  If we attempt to apply it to an input that is
not a natural number, Disco will complain:

::

   Disco> quadruplePlusOne(-3)

Example: factorial
------------------

As another example, let's implement the *factorial* function.  Recall
that the factorial of n is the product of all the natural numbers
from 1 up to n, that is, :math:`n! = n \cdot (n-1) \cdot (n-2) \cdot \dots
\cdot 1`.  Another way to write this is :math:`n! = n \cdot (n-1)!`. If
we add a "base case" :math:`0! = 1`, this becomes a perfectly viable way
to *define* factorial.  We could transcribe it into Disco as follows:

::

    fac : N -> N
    fac(0) = 1
    fac(n) = n * fac (n-1)

Unfortunately, this is a type error. The reason is that since the
input to ``fac`` is supposed to be a natural number, that means
:math:`n-1` must be a natural number, but it cannot be since it uses
subtraction. We can use the ``.-`` operator instead, which works on
natural numbers.  It is like subtraction but with a lower bound of
zero.

::

    fac : N -> N
    fac(0) = 1
    fac(n) = n * fac (n .- 1)

Exercises
---------

* Define a Disco function which returns one more than three times its
  input.  What types can you give it?
