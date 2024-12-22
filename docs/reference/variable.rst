Variables
=========

A *variable* is a name given to some value.  Variable names can
contain lowercase and uppercase letters, digits, underscores (``_``)
and apostrophes, with the only restriction that a name must start with
a letter.  For example, ``myHorse3``, ``some_name``, and ``X__17'x'_``
are all valid variable names.

To define a variable, one must first use a :doc:`type signature
<type-sig>` to declare its :doc:`type <types>` on a line by itself, like

::

   variable_name : type

where ``type`` is replaced by whatever type the variable should have.
The value of the variable can then be :doc:`defined <definition>`
using an ``=`` sign, like

::

   variable_name = expr

where ``expr`` represents an arbitrary :doc:`expression <expression>`.  (There is
also special syntax available for defining :doc:`functions <function>`.)  For example:

::

   my_variable : Z
   my_variable = 2 * 7 + 9

The above code defines the variable ``my_variable`` with the type
``Z`` (*i.e.* an :doc:`integer <integer>`) and the value 23.
