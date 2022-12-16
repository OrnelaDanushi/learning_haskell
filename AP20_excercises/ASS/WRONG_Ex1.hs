module Ex1
( ListBag (..)
, wf
, empty
, singleton
, fromList
, isEmpty
, mul
, toList
, sumBag
) where
-- creating our own module exporting public functions
-- and the data type with its constructor


import qualified Data.Map as M
-- and not import Data.Map otherwise there are ambiguity with the 
-- following implementation of empty, singleton etc. that have to be called 
-- as Main.empty, Main.singleton "" etc from ghci


-- PART 1
-- Implement a type constructor providing the functionalities of msets.

-- The implementation must be based on the following concrete Haskell definition
-- of the ListBag type constructor, which contains a list of pairs whose 
-- first component is the actual element of the mset
-- second component is its multiplicity, that is the number of occurrencies of such element in the mset.
data ListBag a = 
    LB [(a, Int)]  --LB :: [(a, Int)] -> ListBag a 
    deriving (Show, Eq)

-- A ListBag is well-formed if it does not contain two pairs (k, v) and (k', v') with k=k' (*)
-- NOTE: to obtain this, it must be a Map k v from Data.Map converted in a [(k, v)]
-- where k is the type of the keys and v is the type of the values in a map 
-- An example of such a function is toList, that just takes a mapping and converts it to
-- an associative list.
--myToList :: Map k a -> [(k, a)]       --is not useful for the ListBag type
--myToList :: Map a Int -> ListBag a    --if the import of Data.Map is not qualified
myToList :: M.Map a Int -> ListBag a  
myToList myMap = LB ( M.toList myMap )

-- But there must be implemented also the counterpart, so the 
-- conversion from an association list into a map.
--myFromList :: Ord k => [(k, a)] -> Map k a  
--myFromList :: Ord k => ListBag k -> Map k Int
myFromList :: Ord k => ListBag k -> M.Map k Int
myFromList (LB myLB) = M.fromList myLB

-- TEST
list1 = LB [("ciao",1), ("come",2), ("stai", 3)]                --ordered and not repeated
list2 = LB [("ciao",1), ("come",2), ("come", 3), ("stai", 3)]   --ordered and repeated
list3 = LB [("come",1), ("ciao",2), ("stai", 3)]                --not ordered and not repeated
list4 = LB [("come",1), ("ciao",3), ("ciao", 2), ("stai", 3)]   --not ordered and repeated

-- doing so, I get ordered and the last of the repeated element
test_myToList1 = myToList (myFromList list1)    --returns LB [("ciao",1),("come",2),("stai",3)]    
test_myToList2 = myToList (myFromList list2)    --returns LB [("ciao",1),("come",3),("stai",3)]
test_myToList3 = myToList (myFromList list3)    --returns LB [("ciao",2),("come",1),("stai",3)]
test_myToList4 = myToList (myFromList list4)    --returns LB [("ciao",2),("come",1),("stai",3)]

test_myFromList1 = myFromList list1             --returns fromList [("ciao",1),("come",2),("stai",3)]
test_myFromList2 = myFromList list2             --returns fromList [("ciao",1),("come",3),("stai",3)]
test_myFromList3 = myFromList list3             --returns fromList [("ciao",2),("come",1),("stai",3)]
test_myFromList4 = myFromList list4             --returns fromList [("ciao",3),("come",1),("stai",3)]

test_sameList1 = (list1 == test_myToList1)      --returns True  --well-formed
test_sameList2 = (list2 == test_myToList2)      --returns False --not well-formed
test_sameList3 = (list3 == test_myToList3)      --returns True  --well-formed but I get FALSE for the ordering
test_sameList4 = (list4 == test_myToList4)      --returns False --not well-formed


instance Foldable ListBag where 
  length (LB []) = 0
  length (LB (x:xs)) = 1 + ( length xs)
  foldr f acc (LB []) = acc
  --foldr f acc (LB ( a:as)) = foldr f (foldr f acc a) as --TO CHECK

-- if during the transformation the length remained the same, 
-- but changed only the re-ordering then is well-formed 
test_sameListLen1 = (length list1 == length test_myToList1)      --returns True  --well-formed
test_sameListLen2 = (length list2 == length test_myToList2)      --returns False --not well-formed
test_sameListLen3 = (length list3 == length test_myToList3)      --returns True  --well-formed 
test_sameListLen4 = (length list4 == length test_myToList4)      --returns False --not well-formed

----------------------------------------------------------------------------------------

-- EXERCISE 1
-- Write a well documented and working implementation of msets represented concretely 
-- as elements of the type constructor ListBag.

-- Using the type constructor ListBag, implement the predicate 
-- wf that applied to a ListBag
-- returns True if and only if the argument is well-formed.
-- NOTE: 
-- well-formed here means that there are not duplicated keys in the tuples of the LB list (*)
-- and this is checked as above, how done with test_sameListLen
-- and never mind if the ordering for this check changes
-- Check that the inferred type is: wf :: Eq a => ListBag a -> Bool
-- but, Eq is a subset of Ord typeclass, so the inferred type is correct as well
-- the Ord is caused from the fromList function of the Map 
-- for this, all the lists are also ordered

wf :: Ord a => ListBag a -> Bool
wf (LB lb) = ( length (LB lb) == length (myToList (myFromList (LB lb))))
--wf (LB [])                             --returns True
--the other tests are as test_sameListLen

-- All the operations here that return a ListBag bag must ensure that the result is
-- well-formed, i.e. that wf bag == True

-- Implement the following constructors:
-- NOTE: not enriching with constructors the ListBag type but 
-- providing the implementations as normal functions

-- empty, that returns an empty ListBag
-- NOTE: when called by ghci it must be Main.empty otherwise is in conflict with Data.Map.empty
-- if the import of Data.Map is not qualified
empty :: ListBag a
empty = LB []
--empty                                 --returns LB []

-- singleton v, returning a ListBag containing just one occurrence of element v
singleton :: a -> ListBag a
singleton v = LB [(v, 1)]
--singleton "ciao"                      --reurns LB [("ciao",1)]

-- fromList lst, returning a ListBag containing all and only the elements of lst,
-- each with the right multiplicity
fromList :: Ord a => [a] -> ListBag a
fromList xs = myToList (myFromList (LB [(x,l) | x <- xs, l <- [sum [1 | m <- xs, m==x ]]]))
--fromList "ciao"                       --returns LB [('a',1),('c',1),('i',1),('o',1)]
--fromList "bella"                      --returns LB [('a',1),('b',1),('e',1),('l',2)]

-- Implement the following operations:
-- isEmpty bag, returning True if and only if bag is empty
{-
-- NOTE: this fails in testEx12 with:
-- Ambiguous type variable `a0' arising from a use of `isEmpty'
-- prevents the constraint `(Eq a0)' from being solved.
-- Probable fix: use a type annotation to specify what `a0' should be.
-- These potential instances exist:
-- instance Eq Ordering -- Defined in `GHC.Classes'
-- instance Eq Integer  -- Defined in `integer-gmp-1.0.2.0:GHC.Integer.Type'
-- instance [safe] Eq a => Eq (ListBag a) -- Defined at Ex1.hs:31:21
-- ...plus 26 others
-- ...plus 28 instances involving out-of-scope types
-- (use -fprint-potential-instances to see them all)

isEmpty :: Eq a => ListBag a -> Bool
isEmpty bag = (bag == empty)
-}

isEmpty :: ListBag a -> Bool
isEmpty (LB []) = True
isEmpty (LB bag) = False

--isEmpty (LB [])                       --returns True
--isEmpty (LB [("ciao",1)])             --returns False

-- mul k bag, returning the multiplicity of k in the ListBag bag, 
-- if k is an element of bag, and 0 otherwise
eliminate :: Num p => Maybe p -> p --ausiliary function which transform a Maybe value in a value
eliminate (Just a) = a
eliminate Nothing = 0
mul :: Ord k => k -> ListBag k -> Int
mul k bag = eliminate(  M.lookup k (myFromList bag) )
--mul ("come") (LB [("ciao",1), ("come",2), ("stai", 3)])       --returns 2
--mul ("coe") (LB [("ciao",1), ("come",2), ("stai", 3)])        --returns 0

-- toList bag, that returns a list containing all the elements of the ListBag bag,
-- each one repeated a number of times equal to its multiplicity
toList :: ListBag a -> [a]
toList (LB bag) = concat ( map (\(x,y) -> take y (repeat x)) bag )
--toList (LB [("ciao",2),("ornela",3)]) --returns ["ciao","ciao","ornela","ornela","ornela"]
--toList (LB [])                        --returns []


-- sumBag bag bag', returning the ListBag obtained by adding all the elements of 
-- bag' to bag
eliminateNeg (Just a) = a
eliminateNeg (Nothing) = -1

head' :: ListBag a -> (a, Int)
head' (LB bag) = head bag

tail' :: ListBag a -> [(a, Int)]
tail' (LB bag) = tail bag

sumBag' :: Ord a => ListBag a -> ListBag a -> M.Map a Int
sumBag' bag (LB []) = myFromList bag
sumBag' bag bag' =
  let x = fst (head' bag') in
  let y = snd (head' bag') in
  let res = eliminateNeg (M.lookup x (myFromList bag)) in
  if (res == -1 )
    then M.union (myFromList (LB [(x,y)])) (sumBag' bag (LB (tail' bag')))
    else M.union (myFromList (LB [(x, y+res)])) (sumBag' bag (LB (tail' bag')))

sumBag :: Ord a => ListBag a -> ListBag a -> ListBag a
sumBag bag bag' = myToList (sumBag' bag bag')
--sumBag (LB []) (LB [])                                  --returns LB []
--sumBag (LB [("ciao",1)]) (LB [])                        --returns LB [("ciao",1)]
--sumBag (LB [("ciao",1)]) (LB [("ornela",2),("ciao",2)]) --returns LB [("ciao",3),("ornela",2)]

----------------------------------------------------------------------------------------