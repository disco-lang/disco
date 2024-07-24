Strings
=======

A string is a sequence of zero or more :doc:`characters <char>`, used
to represent some kind of text or message.  In fact, a string is
really just a :doc:`list <list>` of :doc:`characters <char>`, that is,
a value of type ``List(Char)``.

A string can be written using the usual syntax for a list of
characters, that is,

::

   mystr : List(Char)
   mystr = ['h', 'e', 'l', 'l', 'o']

However, this is tedious, so Disco also provides special syntax for
strings using double quote marks:

::

   mystr2 : List(Char)
   mystr2 = "hello"

In fact, these are exactly the same; the only difference is that
``mystr2`` uses more convenient syntax:

::

   Disco> mystr == mystr2
   true

A string can be empty; the empty string is written ``""``.  Note that
strings can also contain :doc:`escape sequences <char>`. For example,
``"don\'t \"go\"\naway"``.

Note the difference between ``'x'`` and ``"x"``:

- ``'x'`` is a single character, *i.e.* a value of type ``Char``.
- ``"x"`` is a string, *i.e.* a value of type ``List(Char)``, which
  just happens to have only a single character.

print command
-------------

When a string value is the result of an expression typed at the
:doc:`REPL <REPL>`, it will be displayed in double quotes, with escape
sequences to represent special characters.  If you want the contents
of the string to actually be printed on the screen, interpreting
special characters appropriately, you can use the ``:print`` command.
For example:

::

   Disco> "don\'t \"go\"\naway"
   "don't \"go\"\naway"
   Disco> :print "don\'t \"go\"\naway"
   don't "go"
   away

This can be useful to *e.g.* produce formatted output with multiple
lines.
