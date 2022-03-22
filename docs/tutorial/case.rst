
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

.. literalinclude:: example/case.disco
   :language: idris
   :caption:

The entire expression is surrounded by ``{? ... ?}``; the curly braces
are reminiscent of the big brace following :math:`f(x) = \dots` in the
standard mathematical notation, but we don't want to use plain curly
braces (since those will be used for sets), so question marks are
added (which are supposed to be reminiscent of the fact that case
expressions are about asking questions).

Case syntax and semantics
=========================

More formally, the syntax of a case expression consists of one or more
*branches*, separated by commas, enclosed in ``{? ... ?}``.
(Whitespace, indentation, *etc.* formally does not matter, though
something like the style shown in the example above is encouraged.)

Each *branch* consists of an arbitrary expression followed by zero or
more *guards*.  When a case expression is evaluated, each branch is
tried in turn; the first branch which has *all* its guards succeed is
chosen, and the value of its expression becomes the value of the
entire case expression.  In the example above, this means that first
``x < 0`` is evaluated; if it is true then ``x + 2`` is chosen as the
value of the entire case expression (and the rest of the branches are
ignored).  Otherwise, ``0 <= x < 10`` is evaluated; and so on.

There are three types of guards:

* A *boolean guard* has the form ``if <expr>`` or ``when <expr>``, where ``<expr>`` is an
  expression of type ``Bool``.  It succeeds if the expression
  evaluates to ``true``.  There is no difference between ``if`` and
  ``when``; they are simply synonyms.
* A *pattern guard* has the form ``if <expr> is <pattern>``, or ``when <expr> is <pattern>``.  It succeeds
  if the expression ``<expr>`` matches the pattern ``<pattern>``.
* For convenience, the special guard ``otherwise`` is equivalent
  to ``if true``.

Here is an example using both boolean and pattern guards:

.. literalinclude:: example/case-pattern.disco
   :language: idris
   :caption:

Here is the result of evaluating ``g`` on a few example inputs:

::

    Disco> g(3,9)
    0
    Disco> g(4,3)
    -100
    Disco> g(16,15)
    31

When a pattern containing variables matches, the variables are bound
to the corresponding values, and are in scope in both the branch
expression as well as any subsequent guards.  In the example above,
when the pattern ``(x,y)`` matches ``p``, both ``x`` and ``y`` may be
used in the branch expression (``x + y`` in this case) as well as in
the second guard ``if x > 5 or y > 20``.  That is, the guards in this
branch will only succeed if ``p`` is of the form ``(x,y)`` and either
``x > 5`` or ``y > 20``, in which case the value of the whole case
expression becomes the value of ``x + y``; for example, ``g(16,15) =
31``.

.. warning::

   Be careful not to get a Boolean guard using ``==`` confused with a
   pattern guard using ``is``. The difference is in how variables are
   handled: boolean guards can only use existing variables; pattern
   guards create new variables.  For example, ``... when p is (x,y)``
   matches a tuple ``p`` and gives the names ``x`` and ``y`` to the
   components.  On the other hand, ``... if p == (x,y)`` will probably
   complain that ``x`` and ``y`` are undefined---unless ``x`` and
   ``y`` are already defined elsewhere, in which case this will simply
   check that ``p`` is exactly equal to the value ``(x,y)``.  Use a
   boolean guard when you want to check some condition; use a pattern
   guard when you want to take a value apart or see what it looks
   like.

Function pattern-matching
=========================

As we have already seen, functions can be defined via multiple
clauses and pattern-matching.  In fact, any such definition simply
desugars to one big case expression.  For example, the ``gcd`` function
shown below actually desugars to something like ``gcd2``:

.. literalinclude:: example/function-desugar.disco
   :language: idris
   :caption:

Arithmetic patterns
===================

Disco supports `arithmetic patterns`, in which arithmetic expressions
involving numeric constants, variables, and arithmetic operations can
be used as patterns.  A few examples are shown below.

.. literalinclude:: example/arith-pattern.disco
   :language: idris
   :caption:

::

   Disco> :load example/arith-pattern.disco
   Loading arith-pattern.disco...
   Loaded.
   Disco> map(h, [0 .. 10])
   [1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5]
   Disco> isHalf(3/2)
   true
   Disco> isHalf(4/2)
   false
   Disco> isHalf(17)
   false
   Disco> isHalf(5/(-2))
   true

In short, an arithmetic pattern can contain:

* variables
* natural number constants
* unary negation
* addition, subtraction, multiplication, and division

In most cases, an arithmetic pattern may contain at most one variable;
for example, using ``x + y`` as an arithmetic pattern is an error,
since the resulting values of ``x`` and ``y`` would be ambiguous.  The
one exception is when matching on an expression containing a division
operator, in which case the two patterns are used to separately match
on the numerator and denominator of the value being matched.

The behavior of arithmetic patterns depends on the type being matched.
Generally speaking, matching will succeed if there is a unique value
of the same type that can be assigned to the variable such that the
pattern is equal to the value being matched.  For example, when
matching on natural numbers, the pattern ``2k+3`` will match only odd
numbers greater than or equal to 3, since those are the only numbers
which result from assigning a natural number value to ``k``.  Matching
``2k+3`` against an integer will match all odd integers; matching it
against a rational number will always match.

An arithmetic pattern need not contain any variables, in which case it
is the same as just matching on a particular constant.

At the moment, arithmetic patterns do not support exponentiation,
though that could be a nice thing to add (but surely contains many
pitfalls).
