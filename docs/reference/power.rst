Power set
=========

The *power set* of a :doc:`set <set>` is the set of all possible
subsets.  It can be computed using the ``power`` function, which takes a
``Set(T)`` and returns a ``Set(Set(T))``:

::

   Disco> power({1,2,3})
   {{}, {1}, {1, 2}, {1, 2, 3}, {1, 3}, {2}, {2, 3}, {3}}
   Disco> power(set("hi"))
   {{}, {'h'}, {'h', 'i'}, {'i'}}
   Disco> power({})
   {{}}
   Disco> power(power({}))
   {{}, {{}}}
