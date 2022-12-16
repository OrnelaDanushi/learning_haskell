-- experiment with class constructors by adding some functions to the module developed for Ex1
import Ex1 

----------------------------------------------------------------------------------------

-- define an instance of the constructor class Foldable for the constructor ListBag of Ex1. 
-- To this aim, choose a minimal set of functions to be implemented,
-- as described in the documentation of Foldable.
-- Intuitively, folding a ListBag with a binary function should apply the function
-- to the elements of the mset, ignoring the multiplicities.

instance Foldable ListBag where 

  length (LB [])     = 0
  length (LB (x:xs)) = 1 + (length (LB xs))

  foldr f acc (LB [])      = acc
  --foldr f acc (LB (x:xs))  = f (fst x) (foldr f acc (LB xs))
  foldr f acc (LB ((x,y):xs))  = f x (foldr f acc (LB xs))

  foldl f acc (LB [])     = acc                  
  foldl f acc (LB ((x,y):xs)) = foldl f (f acc x) (LB xs)


{-
-- https://wiki.haskell.org/Fold
instance Foldable [] where

  -- if the list is empty, the result is the initial value acc; else
  -- apply f to the first element and the result of folding the rest
  foldr f acc []      = acc
  foldr f acc (x:xs)  = f acc (foldr f acc xs)

  -- if the list is empty, the result is the initial value; else
  -- we recurse immediately, making the new initial value the result
  -- of combining the old initial value with the first element.
  foldl f acc []     = acc                  
  foldl f acc (x:xs) = foldl f (f acc x) xs
-}

----------------------------------------------------------------------------------------

-- define a function mapLB that takes a function f :: a -> b and a ListBag of type a as an
-- argument, and returns the ListBag of type b obtained by applying f to all the elements of
-- its second argument

{-
-- the application from t to b happens but is not on a LB data structure
mapLB :: (t -> b) -> [(a, t)] -> [(a, b)]
mapLB f bag = map (\(x,y) -> f (x, y)) bag
-}

mapLB :: ((a1, Int) -> (a2, Int)) -> ListBag a1 -> ListBag a2
mapLB f (LB bag) = LB (map (\(x,y) -> f (x, y)) bag)
--mapLB (id) (LB [("ciao",1),("ehi",2)])   returns LB [("ciao",1),("ehi",2)]

----------------------------------------------------------------------------------------

-- explain (in a comment in the same file) why it is not possible to define an instance of Functor
-- for ListBag by providing mapLB as the implementation of fmap

instance  Functor ListBag where
  fmap f (LB bag) = mapLB f (LB bag)

{-
knowing that, 
instance Functor [] where 
  fmap f [] = []
  fmap f (x:xs) = f x : fmap f xs

then, 
instance  Functor ListBag where
  fmap f (LB bag) = mapLB f (LB bag) 

we get the error:
* Occurs check: cannot construct the infinite type: b ~ (b, Int)
  Expected type: (a, Int) -> (b, Int)
  Actual type: a -> b
* In the first argument of `mapLB', namely `f'
  In the expression: mapLB f (LB bag)
  In an equation for `fmap': fmap f (LB bag) = mapLB f (LB bag)
* Relevant bindings include
  f :: a -> b (bound at Ex2.hs:63:8)
  fmap :: (a -> b) -> ListBag a -> ListBag b (bound at Ex2.hs:63:3)
|
|   fmap f (LB bag) = mapLB f (LB bag)


As said in https://wiki.haskell.org/Functor,
'An abstract datatype f a, which has the ability for its value(s) to be mapped over, 
can become an instance of the Functor typeclass. That is to say, a new Functor, f b, 
can be made from f a by transforming all of its value(s), whilst leaving the structure of f 
itself unmodified.
Declaring f an instance of Functor allows functions relating to mapping to be used on structures 
of type f a for all a.'
To allow this, Functors are required to obey certain laws in regards to their mapping, and,
ensuring instances of Functor obey these laws means the behaviour of fmap remains predictable.
They have to be tested manually, since aren't enforced by Haskell automatically.

1. identity
fmap id = id
where id places the role of the f parameter in fmap

2. composition or (->) r or functions are functors
fmap (f . g) == fmap f . fmap g   -- or fmap = (.)
we can think of fmap not as a function that takes one function and a functor and returns a functor, 
but as a function that takes a function and returns a new function that's just like the old one, 
only it takes a functor as a parameter and returns a functor as the result. 
It takes an a -> b function and returns a function f a -> f b. This is called lifting a function. 

if an instance is a Functor instance then it must obey the 2 laws 
but, if the 2 laws are obeyed by an instance, it doesn't mean it is a Functor instance

http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass
Functor is a typeclass for things that can be mapped over, with only typeclass method: fmap
the function f must be applied to one type t to another type b
and, the second argument has to be a constructor that takes one type parameter (not a concrete type)
(it has to have a kind of * -> *, which means that it has to take exactly one concrete 
type as a type parameter.)
so, fmap takes a function from one type to another and a functor applied with one type 
and returns a functor applied with another type
in this case, the functor is the ListBag

http://learnyouahaskell.com/functors-applicative-functors-and-monoids
fmap, which has a type of fmap :: (a -> b) -> f a -> f b. 
It says: give me a function that takes an a and returns a b 
and a box with an a (or several of them) inside it and I'll give you a box with a b 
(or several of them) inside it. It kind of applies the function to the element inside the box.
-}

