REPL commands
=============

The main way of interacting with Disco is through the REPL, *i.e.*
"Read-Eval-Print Loop".  Disco displays a prompt, *Reads* what you
type, *Evaluates* it, *Prints* a result, and then *Loops* to do the
whole thing again.

The prompt looks like this:

::

   Disco>

The main thing you can do is enter :doc:`expressions <expression>`,
which are evaluated.  However, you can also enter :doc:`definitions
<definition>` and :doc:`import statements <import>`, as well as use
various special REPL commands, listed below.  All the special REPL
commands begin with a colon.

``:help``
---------

Show the list of all available commands.

``:load``
---------

Load a ``.disco`` file.  For example,

::

   Disco> :load example/tree.disco
   Loading tree.disco...
   Loaded.

``:reload``
-----------

Reload the file that was most recently loaded with ``:load``.  This is
a helpful option if you are editing a ``.disco`` file.  Instead of
typing out the name of the file again every time you want to test your
latest changes, you can just type ``:reload``.  In fact, you can even
abbreviate it to just ``:r``.

``:names``
----------

Show a list of all the names that are defined, with their types.  This
is useful, for example, if you have just used ``:load`` to load a file
and want to see what definitions it contains. For example:

::

   Disco> :load example/tree.disco
   Loading tree.disco...
   Loaded.
   Disco> :names
   type Tree = Unit + ℕ × Tree × Tree
   leaf : Tree
   node : ℕ × Tree × Tree → Tree
   tree1 : Tree
   treeFold : r × (ℕ × r × r → r) × Tree → r
   treeHeight : Tree → ℕ
   treeSize : Tree → ℕ
   treeSum : Tree → ℕ

``:doc``
--------

Show documentation for something. For example:

::

   Disco> :doc +
   This expression has multiple possible types.  Some examples:
   ~+~ : ℕ × ℕ → ℕ
   ~+~ : ℤ × ℤ → ℤ
   ~+~ : 𝔽 × 𝔽 → 𝔽
   ~+~ : ℚ × ℚ → ℚ
   precedence level 7, left associative

   The sum of two numbers, types, or graphs.

   https://disco-lang.readthedocs.io/en/latest/reference/addition.html

``:table``
----------

The ``:table`` command formats various things as a table.  It can
format lists, or lists of tuples, or lists of lists:

::

   Disco> :table [1,2,3,4,5]
   1
   2
   3
   4
   5

   Disco> :table [(1,1), (2,4), (3,9)]
   1  1
   2  4
   3  9

   Disco> :table [[1,2,3], [4,5,6], [7,8,9]]
   1  2  3
   4  5  6
   7  8  9

It can also display a table of inputs and outputs for any function:

::

   Disco> :table +
   Disco> :table +
     0  0  0
     0  1  1
     1  0  1
     0  2  2
     1  1  2
     2  0  2
     0  3  3
     1  2  3
     2  1  3
     3  0  3
     0  4  4
     1  3  4
     2  2  4
     3  1  4
     4  0  4
     0  5  5
     1  4  5
     2  3  5
     3  2  5
     4  1  5
     5  0  5
     0  6  6
     1  5  6
     2  4  6
     3  3  6
   ...


``:defn``
---------

Show the definition of a name.  For example:

::

   Disco> :load example/tree.disco
   Disco> :defn treeFold
   treeFold : r4 × (ℕ × r4 × r4 → r4) × Tree → r4
   treeFold(x, f, left(■)) = x
   treeFold(x, f, right(n, l, r)) = f(n, treeFold(x, f, l), treeFold(x, f, r))

``:test``
---------

Test a proposition, and display the results.  For example:

::

   Disco> :test forall n:N. n > 5
     - Certainly false: ∀n. n > 5
       Found counterexample:
         n = 0

``:print``
----------

Print a :doc:`string <string>` directly, without double quotes and
escape sequences.

