Disco language extensions
=========================

Disco has several *extensions* to the language which enable special
features.  These features can be enabled with the syntax

::

   using ExtensionName

where ``ExtensionName`` is the name of the desired extension.  Such
``using`` statements must be listed as the *very first thing* in your
``.disco`` file.  They can also be entered at the :doc:`REPL <REPL>`.

Below is the current list of language extensions and what they do.

``NoStdLib``
------------

Normally, a standard library of predefined functions is made available
to any Disco module or at the REPL.  This extension turns off this
automatic standard library import.

This is used for technical reasons in a few of the :doc:`library
<library>` modules, but it is not something most users of Disco will
ever need.

``Primitives``
--------------

Disco has some built-in, primitive functions and operators that are
not normally available to users.  These primitives have names that
begin with a ``$``, which would normally be a syntax error:

::

   Disco> :type $join
   1:7:
     |
   1 | :type $join
     |       ^^^^^
   unexpected "$join"

Enabling this extension allows us to access them.

::

   Disco> using Primitives
   Disco> :type $join
   This expression has multiple possible types.  Some examples:
   $join : List(List(a)) → List(a)
   $join : Bag(Bag(a)) → Bag(a)
   $join : Set(Set(a)) → Set(a)

Other primitives include ``$crash``, ``$isPrime``, ``$factor``,
``$unsafeBagFromCounts``, ``$merge``, ``$frac``, and ``$until``.
