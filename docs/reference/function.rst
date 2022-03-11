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

- Attempting to print a function value will simply result in the type
  of the function being printed as a placeholder:

    ::

       Disco> (\n. 3n+1)
       <ℕ → ℕ>

  This is because once a program is running, Disco has no way in
  general to recover the textual definition of a function.
