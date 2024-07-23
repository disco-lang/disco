Type annotations
================

A *type annotation* is like a :doc:`type signature <type-sig>`, but
instead of being on a line by itself, a type annotation is part of an
:doc:`expression <expression>`.  After any expression we can put a
colon and a type to indicate to Disco what type we want that
expression to be.  For example, if we ask Disco for the type of ``2 +
3``, Disco infers it to be a natural number.

::

   Disco> :type 2 + 3
   2 + 3 : ℕ

However, if we use a type annotation to specify that the ``2`` should
specifically be thought of as an integer, then Disco infers that the
whole expression is also an integer:

::

   Disco> :type (2 : Z) + 3
   (2 : ℤ) + 3 : ℤ

Often, type annotations are useful as documentation: putting a
type annotation on one part of a complex expression helps you better
express your intent, and helps Disco potentially generate better error
messages if you make a mistake.
