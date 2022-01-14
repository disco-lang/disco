Definitions
===========

We can define a :doc:`variable <variable>` to have a certain value
using the syntax

::

   variable = expression

Note that every definition also must have a :doc:`type signature
<type-sig>` before it.  So, for example,

::

   x : Z
   x = -17

declares the variable ``x`` to have the type ``Z`` and to represent
the value ``-17``.  From now on, whenever we use ``x``, it can be
thought of as an abbreviation for the number ``-17``.

Note that the equals sign in Disco really means *mathematical
equality*, like an equation in algebra, and that a variable :doc:`can
have only one definition <dup-def>`.  If you are already familiar with
an imperative language like Python or Java, read the next section for
a comparison with Disco.  If Disco is your first programming language,
you can skip this (though you may read it if you are interested).

Definition vs assignment
------------------------

In many *imperative* languages, variables can be thought of as "boxes"
that store values, and the equals sign means *assignment*.  For
example, in Python,

::

   x = 5
   x = 7

means that we should first assign the value ``5`` to the variable
``x``; then, we *replace* the value stored by ``x`` with ``7``.

In contrast, in Disco (as in some other *functional* languages),
variables are *names* for values, and the equals sign means
*definition*.  In Disco,

::

   x = 5
   x = 7

is an error, because ``x`` cannot be defined as both ``5`` and ``7``;
it cannot be equal to both at the same time.  In other words, it is
like a system of two equations with no solution.
