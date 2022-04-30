Unit type
=========

``Unit`` is a special built-in type with only a single value, which
can be written ``unit`` (or ``■``, U+25A0 BLACK SQUARE).

::

   Disco> :type unit
   ■ : Unit
   Disco> unit
   ■
   Disco> ■
   ■

This is not very useful on its own, but becomes very useful when
combined with :doc:`sum <sum-type>` and :doc:`pair types
<product-type>` to create custom recursive :doc:`algebraic types
<algebraic-types>`.
