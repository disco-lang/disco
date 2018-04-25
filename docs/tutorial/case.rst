
****************
Case expressions
****************

Fundamentally, the only construct available in disco which allows
choosing between multiple alternatives is case analysis using a *case
expression*.  (The other is multi-clause functions defined via
pattern-matching, but in fact that is really only syntax sugar for a
case expression.)

The syntax of case expressions is inspired by mathematical notation
such as

.. math::

   f(x) = \begin{cases}
            x+2           & x < 0 \\
            x^2 - 3x + 2  & 0 \leq x < 10 \\
            5 - x         & \text{otherwise}
          \end{cases}

Here is how one would write a corresponding definition in disco:

.. literalinclude:: code/case-example.disco
   :language: idris
   :caption:

The entire expression is surrounded by ``{? ... ?}``; the curly braces
are reminiscent of the big brace following :math:`f(x) = \dots` in the
standard mathematical notation, but we don't want to use plain curly
braces (since those will be used for sets), so question marks are
added (which remind us that case expressions are really all about
asking questions).

More formally, the syntax of a case expression consists of one or more 
