There is nothing named x
========================

Something in your program, or something you typed at the prompt,
refers to a variable name which is not defined.

- Did you spell the variable name correctly? (Remember that
  capitalization matters!)

- Did you forget to ``import`` a module which defines the variable you
  want?

- Are you trying to refer to a variable outside of the context in
  which it is defined?  For example, a parameter to a function can
  only be used inside the function itself.

- Did you forget to put double quotes around a string, for example,
  ``hello`` instead of ``"hello"``?

If you got this error due to something else not on the list above,
please `add it as a suggestion`_!

.. _`add it as a suggestion`: https://github.com/disco-lang/disco/issues/new
