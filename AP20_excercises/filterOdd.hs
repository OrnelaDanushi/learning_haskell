--write a function filterOdd that given a list xs return a new list obtained from xs by removing
--the elements at odd positions( means the first, third, fifth .. position)
--give as output 2 implementations: one recursive and one using the combinators map/filter/foldr/foldl, where possible

filterOdd :: [a] -> [a]
filterOdd [] = []
filterOdd [x] = [x] --otherwise there is the exception of an empty tail of xs
filterOdd (x:xs) = (x : (filterOdd (tail xs)))

filterOdd_1 xs = case xs of
    [] -> []
    [x] -> [x]
    (x:xs) -> (x : (filterOdd_1 (tail xs)))

filterOdd_2 xs = map (\(x,y) -> y) (filter (\(x,y) -> odd x) ( zip [1 .. ] xs ))

filterOdd_3 xs = map (snd) (filter (\(x,y) -> odd x) ( zip [1 .. ] xs ))

filterOdd_4 xs = map (snd) (filter (\(x,y) -> odd (fst (x,y)) ) ( zip [1 .. ] xs ))
