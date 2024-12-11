Types
=====

Every :doc:`expression <expression>` in disco has a *type*, which
tells us *what kind of value* will result when we evaluate the
expression.  For example, if an expression has type ``N``, it means we
are guaranteed to get a :doc:`natural number <natural>` as a result
once the expression is done being evaluated.

The type of an expression thus represents a *promise* or *guarantee*
about the behavior of a program.  Checking that all the types in a
program match up can also be seen as a way of *predicting* or
*analyzing* the behavior of a program without actually running it.

Each type can be thought of as a collection of values which all have a
similar "shape".

The type of each variable in Disco must be declared with a :doc:`type
signature <type-sig>`.  We can also give Disco hints about the
intended type of an expression using a :doc:`type annotation
<type-annot>`.  We can define our own new types using a :doc:`type
definition <typedef>`.

In some situations, Disco may be willing to accept something of one
type when it was expecting another: specifically, when the given type
is a :doc:`subtype <subtypes>` of the one it was expecting.

.. toctree::
   :maxdepth: 1

   base-types
   function-types
   type-sig
   type-annot
   polymorphism
   type-annot
   algebraic-types
   typedef
   collection-types
   string
   prop
   subtypes
