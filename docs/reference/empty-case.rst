Empty case expressions are not allowed
======================================

Every :doc:`case expression <case>` must have at least one branch.

If you're getting this error, perhaps everything inside a case
expression (``{? ... ?}``) is a :doc:`comment <comment>`?
