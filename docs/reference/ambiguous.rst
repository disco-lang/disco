The name x is ambiguous
=======================

You tried to use a variable which has multiple definitions, so disco
does not know which one you want to use.  The only way this can happen
is if the variable is defined in two different files.  For example,

- The variable is defined both in ``a.disco`` and ``b.disco``, and you
  have both ``import a`` and ``import b`` in your code.

- You have defined the variable in your own code, but it is also
  defined in one of the files you ``import``.

The simplest solution is to rename one of the conflicting
definitions.  If you can't or don't want to do this, you can also make
an "adapter module" to rename a variable without changing the original
file.  For example, suppose we have ``x`` defined in our own file as
well as in ``a.disco``.  We can make a new file named ``b.disco``
which contains the following:

::

   import a

   y :: N
   y = x

Now instead of ``import a`` we can say ``import b``, and now we will
be able to use ``y`` instead of ``x``.
