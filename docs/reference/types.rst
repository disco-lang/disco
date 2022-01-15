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
similar "shape".  The various types in the Disco language include:

- *Base types* are the fundamental types the define the different
  possible kinds of simple data values.
    - :doc:`Booleans <bool>`
    - the :doc:`numeric types <numeric>`, namely :doc:`natural numbers
      <natural>`, :doc:`fractional numbers <fractional>`,
      :doc:`integers <integer>`, and :doc:`rational numbers <rational>`
    - :doc:`characters <char>`


XXX type is a prediction and a guarantee.  Analysis without running
the program.

Note (-1) * (-2) : ℤ even though actually it has value 2 which is a
natural number.
