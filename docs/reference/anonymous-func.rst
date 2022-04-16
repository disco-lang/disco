Anonymous functions
===================

Sometimes, especially when using :doc:`higher-order functions
<higher-order>`, it is convenient to write down a :doc:`function <function>` without
having to give it a name.  This can be done using a so-called "lambda
expression".  In its simplest form, this looks like

::

   \ <var> . <expression>

where ``<var>`` stands for any variable name, and ``<expression>`` stands
for any :doc:`expression <expression>`, which is allowed to use the
variable.  This represents a :doc:`function <function>` which takes
``<var>`` as input, and returns ``<expression>`` as output.

``λ`` (U+033B, GREEK SMALL LETTER LAMBDA) can also be used in place of
``\`` (a backslash is used because it looks kind of like ``λ``).

For example, ``\n. 3n+1`` is the function which returns one more than
three times its input.

::

   Disco> (\n. 3n+1)(6)
   19

- The thing after the lambda or backslash can actually be any
  :doc:`pattern <pattern>`, not just a variable.  For example,

    ::

       \(x,y). x + 2y

  is the function which takes a :doc:`pair <product-type>` of numbers
  as input and returns the sum of the first number and twice the
  second number.

- The variable after the lambda can optionally be annotated with its
  type, as in ``\ <var> : <type> . <expression>``.  For example,

    ::

       \x:Z. x + 5

  is the function of type ``Z -> Z`` which returns 5 more than its
  input.  Without the type annotation, Disco would infer ``\x. x + 5``
  to have type ``N -> N`` instead.
