The shape of two types does not match
=====================================

This is a really horrible, uninformative error message; I'm sorry!  I
hope to improve it soon.

In the meantime, this error is generally caused by the types of
different things not lining up.  For example:

::

   h : Z*Z -> Bool
   h(3) = true

In this example, the type of ``h`` says that ``h`` takes a pair of
integers as input.  However, the definition of ``h`` looks like it
takes a single natural number as input, and these types don't match.

Some tips for pinpointing the error:

- Try narrowing down the source of the error by checking small pieces
  of code by themselves.

- Try asking Disco for the types of various pieces at the prompt to
  see if they match what you think the type should be.  For example,
  Disco can tell us that the type of ``true`` is ``Bool`` (which
  matches the desired output type), but the type of ``3`` is ``N``
  (which does not match).

- Ask for help if you are stuck!
