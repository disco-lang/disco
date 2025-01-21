Functions
=========

.. toctree::
   :maxdepth: 1

   function-types
   pattern
   anonymous-func
   higher-order

A *function* is an input-output relation, that is, we can think of a
function as a machine, or process, that takes inputs and produces
outputs according to some rule(s).  For each element of the *domain*,
or input type, a function specifies a single element of the
*codomain*, or output type.

The :doc:`type of a function <function-types>` with domain ``A`` and
codomain ``B`` is written ``A -> B`` (or ``A → B``).

Two simple examples of functions are shown below.

::

   f : N -> N
   f(n) = 3n + 1

   g : N * N -> Q
   g(x,y) = f(x) / (y - 1)

The function ``f`` takes :doc:`natural numbers <natural>` as input and
produces natural numbers as output; for a given input ``n`` it
produces the output ``3n + 1``.

The function ``g`` takes :doc:`pairs <product-type>` of natural
numbers as input, and produces :doc:`rational numbers <rational>`;
given the pair ``(x,y)``, it produces ``f(x) / (y - 1)`` as output.

- Functions can be given names and defined by :doc:`pattern-matching
  <pattern>`, as in the examples above.

- Functions can also be defined *anonymously*, using :doc:`lambda
  notation <anonymous-func>`.  For example,

    ::

       \n. 3n + 1

  is the function which takes an input called ``n`` and outputs ``3n +
  1``.  This is the same function as the example function ``f`` above.

- Functions can be *applied* to an input by writing the input after
  the name of the function.  Parentheses are not required: for
  example, ``f 5`` is a valid way to apply the function ``f`` to the
  input ``5``.  The space is required in this case to separate the
  function name from the input; ``f5`` would not be valid since it
  would be interpreted as a single variable name.  With the customary
  syntax ``f(5)``, we simply wrap the ``5`` in redundant
  parentheses---any expression can be wrapped in extra parentheses
  without changing its meaning.  In this case a space is not needed
  since the parentheses force Disco to interpret the ``5`` separately
  from ``f``.

  Multi-argument functions follow the same pattern: a "multi-argument"
  function is really a single-argument function that takes a
  :doc:`tuple <product-type>` as input, which must be written with
  parentheses.  So writing ``f (2,3)`` is actually similar to writing
  ``f 5``: a function name followed by an input value.  As is
  customary, we can also omit the space, as in ``f(2,3)``.

- Attempting to print a function value will simply result in the type
  of the function being printed as a placeholder:

    ::

       Disco> (\n. 3n+1)
       <ℕ → ℕ>

  This is because once a program is running, Disco has no way in
  general to recover the textual definition of a function.
