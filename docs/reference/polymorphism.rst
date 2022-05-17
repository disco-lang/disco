Polymorphism
============

Sometimes, we want to express the fact that a certain function will
work for *any* input type at all.  For example, consider a function to
find the length of a list of natural numbers:

::

   lengthN : List(N) -> N
   lengthN([]) = 0
   lengthN(_ :: xs) = 1 + lengthN(xs)

Or how about a function to find the length of a list of rational
numbers:

::

   lengthQ : List(Q) -> Q
   lengthQ([]) = 0
   lengthQ(_ :: xs) = 1 + lengthQ(xs)

It is easy to see that ``lengthN`` and ``lengthQ`` are identical
except for their types, and that it is going to be very tedious if we
have to write a different version of this function for every possible
element type.  The length of a list does not depend on
the elements at all, so we would like to be able to define it once and
for all, in a way that will work for any type of list.

Indeed, we can do exactly that, by using a :doc:`type variable
<variable>` (any name that starts with a lowercase letter) in place of
the concrete element type:

::

   length : List(a) -> N
   length([]) = 0
   length(_ :: xs) = 1 + length(xs)

Here, the variable ``a`` can stand for any type.  This expresses both
a *requirement* that the definition of ``length`` does not care what type
``a`` is (and disco will actually check to make sure that is the
case), and a *promise* that ``length`` can be used on any particular
list.  For example,

::

   Disco> length [1,2,3]
   3
   Disco> length [True, False]
   2

On the other hand, suppose we tried to define this function, which
adds one to every element of a list:

::

   incr : List(a) -> List(a)
   incr([]) = []
   incr(x :: xs) = (x + 1) :: incr(xs)

Disco does not accept this definition.  The problem is that although
it promises that it will work on lists of any type, it doesn't: it
tries to add one to the elements, and adding 1 is only something that
works for some types.  For example, it doesn't make sense to add 1 to
every element in a list of Booleans.  ``incr`` will work fine if we
give it a more specific type, such as ``List(N) -> List(N)``.

Another good example is *function composition*, which takes two
functions and connects the output of one function to the input of the
other, creating a new function representing the "pipeline" of doing
one function then the other.  The input and output types of the
functions don't matter at all --- other than the fact that the output
type of the one function has to match the input type of the other.  We
can write it as follows:

::

   compose : (b -> c) * (a -> b) -> (a -> c)
   compose(f,g) = \x. f(g(x))

``a``, ``b``, and ``c`` can all stand for different types (although
they are not *required* to be different).  Notice, however, that the
input type of the first function is ``b``, and the output type of the
second function is also ``b``---hence no matter what type ``b``
represents, they must be the same.  The function that results takes an
input of type ``a`` and ultimately produces an output of type ``c``
after running the input through both functions.

Note that :doc:`type definitions <typedef>` can also be polymorphic;
see that page for more information.
