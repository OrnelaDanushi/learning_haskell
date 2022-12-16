--write a function myReplicate that given an integer n and a value v
--returns a list of length n intializad with v, namely all elements are equal to v
--give as output 2 implementations: one recursive and one using the combinators map/filter/foldr/foldl, where is possible

--there is also a predefinite function in haskell which makes the same, called replicate
--notAllowed n v = replicate n v

--myReplicate 3 3 returns [3,3,3]
--myReplicate 3 'f' returns "fff"

module MyReplicate where 

myReplicate :: (Eq t, Num t) => t -> a -> [a] --it is not necessary
myReplicate 0 v = []
myReplicate n v = v : (myReplicate (n-1) v)

--the type of the following is the same as the previous

--const v : function which takes an argument and writes it 
--with filter, foldr, is not possible to make this ex
myReplicate1 n v = map (const v) [1..n]

myReplicate2 n v = take n (repeat v)

myReplicate3 n v = myReplicate3' n []
  where
    myReplicate3' 0 xs = xs
    myReplicate3' n xs = myReplicate3' (n-1) (v:xs)

myReplicate4 n v = let myReplicate4' 0 xs = xs
                       myReplicate4' n xs = myReplicate4' (n-1) (v:xs)
  in myReplicate4' n []
  
myReplicate5 n v = map (\_ -> v) [1..n]

myRep n v = take n (map (\_ -> v) [1..])
myRepl n v = take n [v,v..]
(|>) a f = f a
myRepli n v = repeat v |> take n 


--pay attention to the identetation: the calls must be aligned on the same colon  