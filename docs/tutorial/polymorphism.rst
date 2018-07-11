
************
Polymorphism
************

Disco also supports polymorphic functions and let expressions. This is best demonstrated by an example.

Generalizing using Polymorphism
===============================

Consider the task of incrementing each number in a list of natural numbers. In order to accomplish this task using Disco, you could simply write the following function:

::
	
	incr1 : List N -> List N
	incr1 [] = []
	incr1 (a :: as) = (a + 1) :: (incr as)

::

	Disco> incr [1,2,3]
	[2,3,4]

Now consider the task of producing a list of singleton lists containing each element from a provided list of Integers. The following function would suffice:

::

	single1 : List Z -> List (List Z)
	single1 [] = []
	single1 (a :: as) = [a] :: (single as)

:: 

	Disco> single [-1,2,-3]
	[[-1],[2],[-3]]

Do you see the similarities between the two functions? Both functions take a list, apply a function to each element in the list, concatenate the results into a new list, and return this new list. This pattern emerges quite often, and it would be tedious to have to write this same structure twice for two functions as similar as ``incr1`` and ``single1``. Instead, since generalization is the bread and butter of programming, we can capture the essence of the above pattern with the following polymorphic function:

::
	
	map : (a -> b) -> List a -> List b
	map _ [] = []
	map f (a :: as) = f a :: (map as)
	
The function ``map`` is a polymorphic, higher-order function which takes as input a function, ``f``, which transforms values of type ``a`` to values of type ``b``, a list of elements of type ``a``, and returns a list of elements of type ``b`` by applying ``f`` to each element in the input list. Note that map will work for any types, ``a`` and ``b``, so we call ``a`` and ``b`` "type variables". Therefore, you might read the type of ``map`` as:

::
	
	-- map : for all types a and b, (a -> b) -> List a -> List b

Now we can rewrite the functions ``incr1`` and ``single1`` as follows:

::

	incr2 : List N -> List N
	incr2 l = let f = \(x : N). x + 1 in map f l

::

	single2 : List Z -> List (List Z)
	single2 l = let f = \(x : Z). [x] in map f l

It's a good idea to try these functions out and verify that they produce the same outputs as their non-generic counterparts.

Note that the definition of ``single2`` doesn't require it's argument to be a list of Integers. Therefore, we can make ``single2`` polymorphic by changing its' type annotation apporpriately. We can also remove the type ascription from ``x`` in the lambda and Disco will infer that the lambda is polymorphic!

::

	single3 : List a -> List (List a)
	single3 l = let f = \x. [x] in map f l

Other common polymorphic, higher-order functions include ``foldr`` and ``filter``:

::

	filter : (a -> Bool) -> List a -> List a
	filter _ [] = []
	filter f (a :: as) = {?
	                       a :: (filter f as)   if f a
          				   filter f as          otherwise
       				 	 ?}

::

	foldr : (a -> b -> b) -> b -> List a -> b
	foldr _ acc [] = acc
	foldr f acc (a :: as) = foldr f (f a acc) as

If you are unfamiliar with these functions, play around with them and see if you can figure out what they do/what pattern they capture! This will help you understand their types.

Note that the following following function would not typecheck in Disco:

::

	invalid : List a -> List a
	invalid [] = []
	invalid (a : as) = (a + 1) :: (invalid as)

This is because our type definition states that ``invalid`` should work lists of all types (due to the type variable ``a``), however, our definition implies that ``invalid`` only works on lists of a numeric type.
