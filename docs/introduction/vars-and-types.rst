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

