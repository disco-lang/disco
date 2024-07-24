Maps
====

Disco has a built-in type of *maps*, or dictionaries, that map keys to
values.  The type of a map is written ``Map(K, V)`` where ``K`` is the
type of the keys and ``V`` is the type of the values.  For example,
``Map(N, List(Char))`` is the type of maps which associate :doc:`natural
number <natural>` keys to :doc:`string <string>` values.

* The most basic way to construct a ``Map`` is to use the built-in
  function ``map : Set(k * a) -> Map(k, a)``.  That is, ``map`` takes
  as input a set of key-value pairs and constructs a corresponding
  ``Map``.

    ::

       Disco> map({})
       map({})
       Disco> map({(7, "hi"), (5, "there")})
       map({(5, "there"), (7, "hi")})

  Notice that ``map`` is also used when printing out values of type
  ``Map``.

* To look up the value associated with a particular key, you can use
  ``lookup : k * Map(k, a) -> (Unit + a)``.  That is, ``lookup`` takes
  a pair of a key and a ``Map``, and looks up that key, returning
  either a ``left(unit)`` if the key was not found, or ``right(a)``
  with the value ``a`` corresponding to the key if it was found.

    ::

       Disco> m : Map(N, List(Char))
       Disco> m = map({(5, "there"), (7, "hi")})
       Disco> lookup(5, m)
       right("there")
       Disco> lookup(6, m)
       left(unit)

* To insert a new key/value pair into a map, or to change the value
  associated with an existing key, you can use the function ``insert :
  k * a * Map(k, a) -> Map(k, a)``.  This function takes three
  arguments: a key, a value, and an existing ``Map``, and returns an
  updated ``Map`` where the given key is now associated to the given
  value.

    ::

       Disco> insert(5, "you", m)
       map({(5, "you"), (7, "hi")})
       Disco> insert(9, "you", m)
       map({(5, "there"), (7, "hi"), (9, "you")})

  Note that calling ``insert`` did not actually change ``m``; it
  simply returned a new map.

* To turn a map back into a set of key/value pairs, use ``mapToSet :
  Map(k, a) -> Set(k * a)``.

    ::

       Disco> mapToSet(m)
       {(5, "there"), (7, "hi")}
