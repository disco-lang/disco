Characters
==========

The type of *characters* is written ``Char``.  Values of this type
represent single characters (*e.g.* a letter, a digit, punctuation,
etc.).  Technically, a ``Char`` value represents any
`Unicode <https://home.unicode.org/>`_ *codepoint*.

Characters are written using single quotes.  For example,

::

   mychar : Char
   mychar = 'g'

Certain special characters can be written using *escape sequences*,
consisting of a backslash followed by another character:

- ``\n`` represents a newline character
- ``\t`` represents a tab character
- ``\'`` represents a single quote
- ``\"`` represents a double quote
- ``\\`` represents an actual (single) backslash character

A sequence of characters is known as a :doc:`string <string>`.
