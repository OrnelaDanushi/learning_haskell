--write a function titlecase that given a string s converts it to 'titlecase' by uppercasing the first letter of every word
--hint: consider using the function words, unwords of the Prelude and the function toUpper of the module Data.Char
--to make accessible this last function in your code use import Data.Char (toUpper)
--give as output 2 implementations: one recursive and one using the combinators map/filter/foldr/foldl, where possible

import Data.Char (toUpper)

titlecase_1 :: String -> [Char]
titlecase_1 "" = unwords []
titlecase_1 s = (toUpper(head (head (words s)))):(tail (head (words s)))++" "++ (titlecase_1(unwords( tail (words s))))

titlecase :: String -> String
titlecase s = unwords (map (\x-> (toUpper (head x)):(tail x)) (words s) )

--così cambia solo la prima ed è l'unica tornata
--(toUpper (head (unwords (take 1 (words s)))))
--(map (\x-> toUpper x ) (take 1 s)) 

--cosi la trasforma tutta in maiuscolo
--(toUpper (head s)):(titlecase_1 (tail s)) --anche se manca il caso di lista vuota
--((map (\x-> toUpper x ) (take 1 s)))++(titlecase_1 (tail s)) --con eccezione per lista vuota
-- map (\x-> toUpper x ) s 
