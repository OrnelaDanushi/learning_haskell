--write a function totalLength that given a list of strings xs, computes the sum of the lengths of the strings starting with the character 'A'
--give as output 2 implementations: one recursive and one using the combinators map/filter/foldr/foldl, where possible

totalLength :: [[Char]] -> Int
totalLength [] = 0
totalLength (x:xs) = if( take 1 x == "A") then ((length x) + (totalLength xs)) else (totalLength xs)

totalLength_1 xs = case xs of
    [] -> 0
    (x:xs) -> if( take 1 x == "A") then ((length x) + (totalLength_1 xs)) else (totalLength_1 xs)

totalLength_2 l = foldl (+) 0 ( map (\x-> length x) (filter (\x-> take 1 x=="A") l) )

-- but remember that for the empty list there is the excpetion
totalLength_3 l = foldl1 (+) ( map (\x-> length x) (filter (\x-> take 1 x=="A") l) )

--with list comprehension
totalLength_4 xs = foldl (+) 0 [  length x | x <- xs, ( take 1 x == "A") ]