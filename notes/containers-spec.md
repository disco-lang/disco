Specification for disco containers
==================================

Syntax
------

```
<container>
  ::= '[' <container-contents> ']'
    | '{' <container-contents> '}'
    | '⟅' <container-contents> '⟆'

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
⟅ 5 .. 100 ⟆
⟅ 3, 3, 3 ⟆
```

Conversion
----------

We have the following Galois connections (actually just
section/retraction pairs):

```
      --- forget order -->     --- forget cardinalities -->
List                       Bag                              Set
      <-- sorted order ---     <-- assign card. 1 to all --
```

Going left and then right in this diagram is the identity.  Going
right and then left is not, in general, the identity, but represents
some sort of (idempotent) canonicalization: `List -> Bag -> List`
is `sort`; `Bag -> Set -> Bag` drops duplicates; `List ->
Set -> List` both sorts and drops duplicates.

There are conversion functions `list`, `bag`, and `set` which convert
any container into the named type according to the above diagram. For
example, `list` can have any of the types

```
list : List a -> List a
list : Bag a -> List a
list : Set a -> List a
```

And similar for `set` and `bag`.  These are of course built-in
primitives, since their types cannot be expressed using the disco type
system.

There is no subtyping among container types.  Although it would in
theory make sense to have the subtyping relationships List < Bag < Set
(converting in this direction gives rise to ringad homomorphisms),
this is not sound in the presence of conversion functions like
`list`.  For example, `list ⟅1,1,2⟆ == [1,1,2]`, but `list (set
⟅1,1,2⟆) == [1,2]`, and with subtyping the latter `set` is allowed to
be silently inserted.

There is also one more built-in conversion function,
```
bagCounts : Bag a -> Set (a * N)
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


Dynamic (runtime) semantics
---------------------------

