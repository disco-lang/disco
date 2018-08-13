Specification for disco containers
==================================

Syntax
------

```
<container>
  ::= '[' <container-contents> ']'
    | '{' <container-contents> '}'

<container-contents>
  ::= empty | <nonempty-container>

<nonempty-container>
  ::= <item> [ <ellipsis> ]
    | <item> <container-end>

<container-end>
  ::= '|' <comprehension>
    | ',' [ <item> (',' <item>)* ] [ <ellipsis> ]

<comprehension> ::= <qual> [ ',' <qual> ]*

<qual>
  ::= <ident> 'in' <term>
    | <term>

<item> ::= <term> [ '#' <natural> ]

<ellipsis> ::= '..' [ <term> ]
```

Syntax examples:

```
{ 'x', 'y', 'z' }
[ 1, 3 .. ]
{ 5 # 3 .. 100 }
[ 'a' # 5, 'b', 'c' # 19 ]
```

Subtyping/conversion
--------------------

The subtyping here is perplexing.  On the one hand it seems that a
hierarchy like `List < Multiset < Set` makes sense, going from more free
to less free.  There are appropriate monoid homomorphisms along each
of these.  On the other hand there is also a sensible monoid
homomorphism `Set -> Multiset`.

I am coming around to the position that there shouldn't be any
container subtyping at all; there should instead be explicit functions
`list`, `set`, `multiset` which will convert *to* the given container
type *from* any other container type.  We have the following Galois
connections (actually just section/retraction pairs):

```
      --- forget order -->          --- forget cardinalities -->
List                       Multiset                              Set
      <-- sorted order ---          <-- assign card. 1 to all --
```

Going left and then right in this diagram is the identity.  Going
right and then left is not, in general, the identity, but represents
some sort of (idempotent) canonicalization: `List -> Multiset -> List`
is `sort`; `Multiset -> Set -> Multiset` drops duplicates; `List ->
Set -> List` both sorts and drops duplicates.

So *e.g.* `list` can have either of the types

```
list : Multiset a -> List a
list : Set a -> List a
```

And similar for `set` and `multiset`.  These will of course have to be
built-in primitives, since their types cannot be expressed using the
disco type system.

There should also be one more built-in conversion function,
```
countSet : Multiset a -> Set (a * N)
```
which turns a multiset into the set of its (element, count) pairs.

Other primitives
----------------

`fold : (a -> b -> b) -> b -> Container a -> b` should work for any
kind of container.  (???)

Typechecking
------------



If a function `f` expects a set, you can't call it with a list, `f
[1,2,3]`, but you could instead say `f (set [1,2,3])`.  It also won't
be possible to write `{ x | x in [1 .. 10], even x }` because that
would mix a set and a list; one would have to write `{ x | x in set
[1..10], even x }` (actually in this case of course one could just
write `x in {1 .. 10}`).

Dynamic (runtime) semantics
---------------------------



