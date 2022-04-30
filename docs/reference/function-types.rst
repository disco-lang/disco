Function types
==============

*Function types* represent :doc:`functions <function>` that take
inputs and yield outputs.  The :doc:`type <types>` of functions that
take inputs of type `A` and yield outputs of type `B` is written ``A
-> B`` (or ``A → B``).

Every function in Disco takes exactly one input and produces exactly
one output.  "Multi-argument" functions can be represented using a
:doc:`pair type <product-type>` as the input type. For example, the
type of a function taking two natural numbers as input and producing a
rational number as output could be written ``N × N → Q``.

Note that if ``A`` and ``B`` are types, then ``A -> B`` is itself a
type, which can be used in all the same ways as any other type. In
other words, functions in Disco are "first-class", which means they
can be used in the same ways as any other values: for example,
functions :doc:`can be given as input to other functions
<higher-order>`, or returned as output, we can have lists of
functions, *etc.*

Function values cannot be :doc:`compared <compare>`, however, because
in general this would require testing the functions on every single
possible input, of which there might be infinitely many.
