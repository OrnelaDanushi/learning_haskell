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


-- PART_1: Implement a TypeConstructor providing the functionalities of msets.
-- The implementation must be based on the following concrete Haskell definition
-- of ListBag, which contains a list of pairs whose 
-- first component is the actual element of the mset
-- second component is its multiplicity, that is the number of occurrencies of such element in the mset.
data ListBag a = 
    LB [(a, Int)]  --LB :: [(a, Int)] -> ListBag a 
    deriving (Show, Eq)



-- EXERCISE_1: Write a well documented and working implementation of msets represented concretely 
-- as elements of ListBag.



-- applied to a ListBag returns True iff the argument is well-formed
-- so if it does not contain two pairs (k, v) and (k', v') with k=k'
-- there are not duplicated keys in the tuples of the LB list

wf :: Eq a => ListBag a -> Bool
wf( LB [] )               = True
wf( LB( (x, _):[] ) )     = True
wf( LB( (x, int):xs ) )
  | x == fst( head xs )   = False 
  | otherwise             =
      let   par1 = wf( LB( (x, int):(tail xs) ) ) 
            par2 = wf( LB( xs ) )
      in ( par1 == par2 ) && (par1 == True)

--wf( LB[("ciao",1),("ornela",1)])              returns True
--wf( LB[("ciao",1),("ciao",1)])                returns False
--wf( LB[("ciao",1),("ornela",1),("ciao",3)])   returns False 




-- Implement the following constructors, not enriching with constructors the ListBag type 
-- but as normal functions


-- empty, returns an empty ListBag
empty :: ListBag a
empty = LB []

--empty                                         returns LB []



-- singleton v, returns a ListBag containing just one occurrence of element v
singleton :: a -> ListBag a
singleton v = LB [(v, 1)]

--singleton "ciao"                              reurns LB [("ciao",1)]



-- fromList lst, returns a ListBag containing all and only the elements of lst,
-- each with the right multiplicity

-- this allows repeated elements, is implemented differently
--fromList xs = (LB [(x,l) | x <- xs, l <- [sum [1 | m <- xs, m==x ]]])
--fromList "ciao"                             returns LB [('a',1),('c',1),('i',1),('o',1)]
--fromList "bella"                            returns LB [('b',1),('e',1),('l',2),('l',2),('a',1)]
-- and not                                            LB [('a',1),('b',1),('e',1),('l',2)]

countIncrementDrop  :: (Eq a1, Num b) => (a1, b, [a1], [a1]) -> (a1, b, [a2], [a1])
countIncrementDrop( x, v, [], xs  )   =  (x, v, [], xs)
countIncrementDrop( x, v, y:ys, xs )
  | x == y                            = countIncrementDrop( x, (v+1), ys, xs)
  | otherwise                         = countIncrementDrop( x, v, ys, y:xs)

generalList :: (Eq a, Num b) => [a] -> [(a, b)]
generalList []                                      = []
generalList [x]                                     = [(x,1)]
generalList (x:xs)  
  | (null toCheck)==True && (null checked)==True    = [(item, count)]
  | (null toCheck)==True && (null checked)==False   = (item, count) : (generalList checked) 
  | otherwise                                       = []
  where (item, count, toCheck, checked) = countIncrementDrop(x, 1, xs, [])

-- NOTE:
-- length list == 0, needs to traverse the whole list, O(n)
-- list == [], yields an Eq constraint on the element type  
-- null list, runs in constant time and has no typeclass constraints

fromList :: Eq a => [a] -> ListBag a
-- fromList :: Ord a => [a] -> ListBag a  NOTE: it is also ok, since is more general 
fromList []   = empty
fromList [x]  = singleton x
fromList xs   = LB (generalList xs)

--fromList "ciao"                             returns LB [('c',1),('o',1),('i',1),('a',1)]
--fromList "bella"                            returns LB [('b',1),('a',1),('e',1),('l',2)]
--fromList "ciaoornelaciao"                   returns LB [('c',2),('o',3),('i',2),('a',3),('r',1),('l',1),('n',1),('e',1)]



-- Implement the following operations:


-- isEmpty bag, returns True iff bag is empty
isEmpty :: ListBag a -> Bool

--isEmpty :: Eq a => ListBag a -> Bool with Eqthis fails testEx12.hs since is not able to infer correctly
--isEmpty bag = (bag == empty) this fails testEx12.hs since is not able to infer correctly

isEmpty ( LB []) = True
isEmpty ( LB xs) = False

--isEmpty( LB [])                             returns True
--isEmpty( LB [(1,2)])                        returns False




-- mul k bag, returns the multiplicity of k in the ListBag bag, 
-- if k is an element of bag, and 0 otherwise
mul :: Eq a => a -> ListBag a -> Int
mul k (LB [])                 = 0
mul k (LB ( (x,y):[] ))
  | k==x                      = y
  | otherwise                 = 0
mul k (LB ( (x,y):xs ))
  | k==x                      = y
  | otherwise                 = mul k (LB xs)

--mul ("come") (LB [("ciao",1), ("come",2), ("stai", 3)])       returns 2
--mul ("coe") (LB [("ciao",1), ("come",2), ("stai", 3)])        returns 0



-- toList bag, returns a list containing all the elements of the ListBag bag,
-- each one repeated a number of times equal to its multiplicity
toList :: ListBag a -> [a]
toList (LB bag) = concat ( map (\(x,y) -> take y (repeat x)) bag )

--toList (LB [("ciao",2),("ornela",3)])     returns ["ciao","ciao","ornela","ornela","ornela"]
--toList (LB [])                            returns []
--toList( fromList "ciaoornelaciao")        returns "ccoooiiaaarlne"


-- sumBag bag bag', returns the ListBag obtained by adding all the elements of bag' to bag
sumBag :: Eq a => ListBag a -> ListBag a -> ListBag a
sumBag bag bag' =  LB( generalList( (toList bag)++(toList bag') ))

--sumBag (LB []) (LB [])                                  --returns LB []
--sumBag (LB [("ciao",1)]) (LB [])                        --returns LB [("ciao",1)]
--sumBag (LB [("ciao",1)]) (LB [("ornela",2),("ciao",2)]) --returns LB [("ciao",3),("ornela",2)]