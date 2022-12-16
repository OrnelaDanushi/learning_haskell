--write a function sumOdd that given a list of integers computes the sum of the values that are odd
--consider the functions odd and even
--give as output 2 implementations: one recursive and one using the combinators map/filter/foldr/foldl, where possible

--sumOdd [3,5,6] returns 8

main = print (sumOdd [])

sumOdd :: Integral p => [p] -> p 
sumOdd [] = 0
--sumOdd (x:xs) = if (odd x) then (x + (sumOdd xs)) else (sumOdd xs)
sumOdd (x:xs) = if (not (even x)) then (x + (sumOdd xs)) else (sumOdd xs)

sumOdd1 l = case l of
     [] -> 0
     (x:xs) -> if (odd x) then (x + (sumOdd1 xs)) else (sumOdd1 xs)

sumOdd2 l = foldl (+) 0 (filter odd l)

sumOdd3 l = foldl1 (+) (filter odd l)