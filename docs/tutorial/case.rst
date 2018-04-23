
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

Here is the disco syntax corresponding to the above definition:

.. literalinclude:: code/case-example.disco
   :language: idris
   :caption:
