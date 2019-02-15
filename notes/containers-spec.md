Specification for disco containers
==================================

Syntax
------

```
<container>
  ::= '['  <container-contents> ']'
    | '{'  <container-contents> '}'
    | '{#' <container-contents> '#}'

<container-contents>
  ::= empty | <nonempty-container>

<nonempty-container>
  ::= <term> [ <ellipsis> ]
    | <term> <container-end>

<container-end>
  ::= '|' <comprehension>
    | ',' [ <term> (',' <term>)* ] [ <ellipsis> ]

<comprehension> ::= <qual> [ ',' <qual> ]*

<qual>
  ::= <ident> 'in' <term>
    | <term>

<ellipsis> ::= '..' [ <term> ]
```

Syntax examples:

```
{ 'x', 'y', 'z' }
[ 1, 3 .. ]
{# 5 .. 100 #}
{# 3, 3, 3 #}
```

Subtyping/conversion
--------------------

We have the following Galois connections (actually just
section/retraction pairs):

```
      --- forget order -->     --- forget cardinalities -->
List                       Bag                              Set
      <-- sorted order ---     <-- assign card. 1 to all --
```

Subtyping goes from left to right; each of the "forgetful" arrows
corresponds to a subtype relationship, and is a ringad homomorphism.
For example if a function expects a set, one can perfectly well give
it a list or a bag.

There are canonical mappings going in the other direction; however,
these are not ringad homomorphisms (for example, whenever two sets
share common elements, unioning them and then converting to a bag is
not the same as converting to bags first and then doing bag (additive)
union).  Hence these should NOT be subtyping relationships.  Instead,
they will be given by explicit (primitive functions) `bag`, `list`,
and `set`, which convert any container into the respective type.

Going left and then right in this diagram is the identity.  Going
right and then left is not, in general, the identity, but represents
some sort of (idempotent) canonicalization: `List -> Bag -> List`
is `sort`; `Bag -> Set -> Bag` drops duplicates; `List ->
Set -> List` both sorts and drops duplicates.

So *e.g.* `list` can have any of the types

```
list : List a -> List a
list : Bag a -> List a
list : Set a -> List a
```

And similar for `set` and `bag`.  These will of course have to be
built-in primitives, since their types cannot be expressed using the
disco type system.

There should also be one more built-in conversion function,
```
countSet : Bag a -> Set (a * N)
```
which turns a bag into the set of its (element, count) pairs.

Primitives
----------

* `list`, `bag`, `set` (as discussed above)
* `countSet : Bag a -> Set (a * N)`
* `union : Set a -> Set a -> Set a`
* `maxUnion : Bag a -> Bag a -> Bag a`
* `plusUnion : Bag a -> Bag a -> Bag a`
* `intersect`:
    - `Set a -> Set a -> Set a` OR
    - `Bag a -> Bag a -> Bag a` OR
    - `List a -> Set a -> List a` (OR vice-versa)
* `difference`:
    - `Set a -> Set a -> Set a` OR
    - `Bag a -> Bag a -> Bag a` OR
    - `List a -> Set a -> List a` OR
    - `List a -> Bag a -> List a`
* `power`: ??
    - `Set a -> Set (Set a)`
    - `Bag a -> Bag (Bag a)`
    - `List a -> Bag (List a)`
* `subthingy`: ??
    - `Set a -> Set a -> Bool`
    - `Bag a -> Bag a -> Bool`
    - `List a -> List a -> Bool`

Standard library functions
--------------------------

* `length`, `setSize`, `bagSize`
    - Have primitive `size` that turns into one of the above?
* `foldr`
* `sum`, etc. on lists only
    - If you want to e.g. sum a set, have to do  `sum(list(s))` ?

Typechecking
------------

XXX

If a function `f` expects a set, you can't call it with a list, `f
[1,2,3]`, but you could instead say `f (set [1,2,3])`.  It also won't
be possible to write `{ x | x in [1 .. 10], even x }` because that
would mix a set and a list; one would have to write `{ x | x in set
[1..10], even x }` (actually in this case of course one could just
write `x in {1 .. 10}`).

Dynamic (runtime) semantics
---------------------------



To do
-----

- Add type inference for crash (& other prims??)
- Add primitives 'reduce' and 'mapReduce' on containers.
- Add container subtyping
- Fix container comprehensions.
- Write down a specific, concrete list of the container primitives
  we'll have, along with some concrete typing rules.
- Update type checking to emit relevant constraints, in accordance
  with the written typing rules.

- Note, infinite ellipses in sets and bags are now allowed, e.g.

  {1 ..}

  but the only way they could ever be useful is if we switched to a
  deep embedding somehow.

〚〛
