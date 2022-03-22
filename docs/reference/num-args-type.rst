Not enough/too many arguments for the type T
============================================

Some built-in types expect to be given one or more types as arguments (for
example, `List` and `Set` both expect one argument; `Map` expects
two).  You can also define your own types that expect arguments.
These error messages show up when you have given the wrong number of
arguments to a type.  For example:

::

   Disco> t : List
   Error: not enough arguments for the type 'List'.
   Disco> t : List(N,Q)
   Error: too many arguments for the type 'List'.
   Disco> type MyType(a,b,c) = List(a) * Set(b) * List(c)
   Disco> q : MyType(Char,Bool)
   Error: not enough arguments for the type 'MyType'.

