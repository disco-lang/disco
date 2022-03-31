Literal pattern
===============

A literal :doc:`pattern <pattern>` consists of a single specific
value.  For example,

::

   f(5) = "hi"

uses ``5`` as a literal pattern, and means that on the specific input
``5``, the function ``f`` should output ``"hi"``.

Literal patterns can be used with

- The :doc:`unit value <unit>`, *e.g.* ``f(unit) = ...``
- :doc:`Booleans <bool>`, *e.g.* ``f(true) = ...``
- :doc:`Natural numbers <natural>`, *e.g.* ``f(5) = ...``
- :doc:`Integers <integer>`, *e.g.* ``f(-5) = ...``
- :doc:`Rational numbers <rational>`, *e.g.* ``f(1/2) = ...``
- :doc:`Characters <char>`, *e.g.* ``f('x') = ...``
- :doc:`Strings <string>`, *e.g.* ``f("hello") = ...``

Note that ``f(-5) = ...`` and ``f(1/2) = ...`` are technically
:doc:`arithmetic patterns <arith-pattern>` rather than true literal
patterns, but the distinction does not matter very much.
