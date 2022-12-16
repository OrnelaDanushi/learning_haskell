--write a function repl that given a list xs and an integer n return a list containing the elements of xs replicated n times
--use the function myReplicate 
--give as output 2 implementations: one recursive and one using the combinators map/filter/foldr/foldl, where possible

import MyReplicate

repl :: (Eq t, Num t) => [a] -> t -> [a]

repl [] n = []
repl xs 0 = xs

--this is usefull for a pattern matching
--repl (x:xs) n = (myReplicate n x):(repl xs n)
--the : is not correct because it builds lists taking the elements apart
-- 1:[2,3] is legal, whereas [1]:[2,3] is not

repl (x:xs) n = (myReplicate n x)++(repl xs n) 
--the ++ is correct for the list concatenation, it takes 2 lists and appends them together
--[1,2,3]++[4,5,6] is legal, whereas 1++[1,2,3] is not

repl_1 l n = case (l,n) of
    ([],_) -> []
    (xs,0) -> xs
    ((x:xs),n) -> (myReplicate n x)++(repl_1 xs n) 

repl_2 xs n = foldl (++) [] (map (myReplicate n) xs) 
--map returns a list built by applying a function (the first argument) to all items in a list passed as the second argument
--foldl, here applies the concatenation to each element of the list

repl_3 xs n = foldl1 (++) (map (myReplicate n) xs) 

repl_4 xs n = concat (map (myReplicate n) xs) 
--concat takes a list of list and returns a list of the concatenated elements of the list