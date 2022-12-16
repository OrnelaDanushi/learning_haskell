--write a function countVowelPali that given a list of strings xs return the total
--number of vowels in strings that are palindromes
--es: countVowelPali ["anna", "banana", "civic", "mouse"] = 4 (=a+a+i+i)
--give as output 2 implementations: one recursive and one using the combinators map/filter/foldr/foldl, where possible

--palindrome "" = 0
--palindrome s = if (head s == last s) then (1 + (palindrome (take ((length (tail s))-1 ) (tail s) ))) 
--                                     else (palindrome (take ((length (tail s)) -1 ) (tail s)))

--isVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' || c == 'y'
--isVowel s = if elem s ['a','e','i','o','u','y'] then True else False
isVowel c = elem c "aeiouy"

countVowel "" = 0
countVowel str = if ( isVowel (head str) ) then (1 + (countVowel (tail str))) else (countVowel (tail str))

getReverse "" = ""
getReverse s = (last s):(getReverse ((take ((length s )- 1) s)))

countVowelPali :: Num b => [[Char]] -> b
--countVowelPali xs = foldl (+) 0 (map (countVowel ) (filter (\x-> x==(reverse x)) xs))
countVowelPali xs = foldl (+) 0 (map (countVowel ) (filter (\x-> x==(getReverse x)) xs))


countVowelPali_1 [] = 0
countVowelPali_1 (x:xs) = if( x==(getReverse x)) then (countVowel x)+(countVowelPali_1 xs) else (countVowelPali_1 xs)