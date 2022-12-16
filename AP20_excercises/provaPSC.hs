:?

:set +t 
-- to get info about each operators, that are treated like containers, as

-- plus, times .. are left associative
-- the power * is right associative
:t (*)

-- div 7 2 to take the integer part
-- mod 7 2 to take the remaining part
-- 7 / 2 to take floating point number

-- /= is the different operation as 2 /= 3 gives True
:t (>)
:t show -- is a type class, in it there are not functions, so functions cannot be printed into the screan

:t read
read "5" -- here it has no info about the type of 5 rather than it is a string
read "5" :: Int -- now the type is inferred directly
2 + (read "5") -- now the interpreter can deduce automatically the type of 5

:info Int -- gives info about the type class 

let ll = ["hello", "how", "are", "you"]
[[x | x<-string, not(elem x "aeiou")] | string<-ll]

fst (1, 'a')
snd (1, 'a')
zip [1..26] ['a'..'z'] -- makes the cartesian product for each pair of elements

-- which right triangle has integers for all sides and all sides are <= 10 and a perimeter of 24?
[(x,y,z) | x<-[1..10], y<-[1..10], z<-[1..10], x+y+z==24, z^2==x^2+y^2]
-- [(6,8,18),(8,6,10)]

:set +m --allows to write multi line formulas
let f = \x -> x
    g = \y -> y
-- f == g will give an error since is not possible to compare functions, they are both the identity function
let f x = x -- is the same as the previous f

let max a b = if a>b then a else b
let max a b
      | a>b = a
      | b>a = b
-- with max 2 2 it gives an error since is not handled this step in the defenition of the combinations
let max a b
      | a>b = a
      | b>a = b
      | otherwise = a

-- max (2, 3) it gives an error because the defined argument was of of 2 values and not of a pair of values
-- this can be fixed with the carrying and uncarrying feature

-- pattern matching and case analysis with lists
let empty [] = True
    empty (x:xs) = False

let (^^) x y = x^2 + y^2  -- is an infix ooperator as 3 ^^ 4
let infixl ^^
    (^^) x y = x^2 + y^2

let init first last = [f, '.', l, '.'] -- here is defined the result
        where   f = head first
                l = head last

let init first last = [f, '.', l, '.']
        where   (f:_) = first
                (l:_) = last
initi "Paolo" "Rossi"

let what :: String -> String
    what [] = "nothing"
    what xs
        | last xs == '?' = "Question"
        | last xs == '!' = "Exclamation"
        | last xs == '.' = "Sentence"
        | otherwise = "Please repeat"

let what :: String -> String
    what [] = "nothing"
    what xs
        | x == '?' = "Question"
        | x == '!' = "Exclamation"
        | x == '.' = "Sentence"
        | otherwise = "Please repeat"
        where x = last xs
        
what "Hello!"

let fac 0 = 1
    fac n   | n>0 = n * fac(n-1)
            | otherwise = 0

let myApp [] ys = ys
    myApp xs [] = xs
    myApp (x:xs) ys = x (myApp xs ys)
    
let rev [] = []
    rev (x:xs) = (rev xs) ++ [x]
    
let rev xs = revAux xs []
        where   revAux [] aux = aux
                revAux (x:xs) aux = revAux xs (x:aux)

let rev xs = let revAux [] aux = aux
                 revAux (x:xs) aux = revAux xs (x:aux)
             in revAux xs []

flip f b a = f a b

-- area of cylinder
let cyl r h = 2*(pi * r^2) + (2*pi*r*h)

let cyl r h = let base = pi * r^2
                  side = 2*pi*r*h
              in 2*base + side

-- implement replicate n a -> [a,a, ,a]
let repl 0 a = []
    repl n a | n>0 = a : repl (n-1)
             |otherwise = []

-- take n xs
let mytake 0 xs = []
    mytake n [] = []
    mytake n (x:xs) | n>0 = x : (mytake (n-1) xs)
                    | otherwise = []

-- repeat a -> [a,a,a,..]
let myrepeat a = a : (myrepeat a)
take 10 (myrepeat 'a')

-- elem
let myelem a [] = False
    myelem a (x:xs) = (a==x) || myelem a xs
    
-- zip
let myzip [] ys = []
    myzip xs [] = []
    myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

-- given a list return the pairs of adjacent elements
let pairs xs = zip xs (tail xs)

-- check if a list is sortedd using the above functions
let sorted xs = [.. |(x1,x2) <- (pairs xs), x1<=x2]
let sorted xs = null [1 | (x1,x2)<-(pairs xs), x1>x2]

-- given a list and one element return the set of all positions in the list
-- where the element appears, first position of the list in numbered 0
let pos a xs = [posx | (x,posx)<-(zip xs [0..]), x==a]
pos 'e' "this is a sentence without meaning"
[11,14,17,28]

-- insert a new element in a sorted list
let insert a [] = [a]
    insert a (x:xs) | a<=x = a : x : xs
                    | otherwise = x : (insert a xs)

-- implement insertion sort
let isort [] = []
    isort (x:xs) = insert x (isort xs)

-- implement quicksort
let qsort [] = []
    qsort (x:xs) = sortedsmaller ++ (x : sortedlarger)
          where sortedsmaller = qsort [y | y<-xs, y<=x]
                sortedlarger = qsort [y | y<-xs, y>x]

-- map f [x1,x2,...,xn] -> [f(x1),f(x2),...,f(xn)]
let mymap f [] = []
    mymap f (x:xs) = f(x) : mymap f xs

-- I have 3 children, the product of their ages is 72, the sum of their ages is the number of this gate
-- I have to see my eldest child who is in bed with measles (morbillo)
[(x+y+z, x,y,z) | x<-[1..72], y<-[x..72], z<-[y..72], x*y*z==72]

-- filter p [x1,..,xn] -> takes the elements for which the p is true
let myfilter p [] = []
    myfilter p (x:xs) | p x == x : myfilter p xs
                      | otherwise = myfilter p xs

myfilter odd [1..10]
myfilter (\x -> (elem x ['A'..'Z']) ) "hello, THis is AnoTher SenTence"

let qsort [] = []
    qsort (x:xs) = smaller ++ (x:larger)
          where smaller = qsort (filter (<=x) xs)
                larger = qsort (filter (>x) xs)

-- find the largest # under 100'000 that is divisible by 3829
head (filter (\n -> (mod n 3829)==0) [100000,99999..3829] )
head (filter (0==).('mod' 3829)) [100000,99999..3829]

-- generate the list of all positive factors of a number
let factors n = [d | d<-[1..n], (mod n d) == 0]

-- check if a positive number is prime
let prime n = [1,n] == (factors n) 

-- generate the list of all prime numbers
take 100 (prime n)

let primes = filter prime [2..]
take 10 primes

:set +s -- to get some info about memory consuming times and other measures

length [p | p<-[2..4000], prime p] -- takes about 30 seconds
length [p | p<-2:[3,5..4000], prime p] -- discarding all the even numbers, takes about 34 seconds

let sieve [] = []
    sieve (p:xs) = p : (sieve [y | y<-xs, (mod y p )/=0 ])
length (sieve [2..40000]) -- circa 4 seconds

let sieve1 [] = []
    sieve1 (p:xs) = p : [x | x <- sieve1 xs, (mod x p)/=0 ]
length (sieve1 [2..20000]) -- circa 85 seconds

:unset +s
-- fix :: (a -> a) -> a
let fix f = (let x = f x in x)
let fact = \n -> if n<=0 then 1 else n*fact(n-1)
let fact = (\f n -> if n<=0 then 1 else n*f(n-1)) fact -- fact applied to a function f
let gamma = (\f n -> if n<=0 then 1 else n*f(n-1))

let listFun = map (*) [1..]
(listFun !! 5) 8

let listApp :: [a->b] -> a -> [b]
    listApp [] _ = []
    listApp (f:fs) n = (f n) : (listApp fs n)

let zipFun :: [a->b] -> [a] -> [b]
    zipFun [] _ = []
    zipFun _ [] = []
    zipFun (f:fs) (x:xs) = (f x) : (zipFun fs xs)
take 10 (zipFun listFun [1..])

-- sum [] = 0
-- sum (x:xs) = x + (sum xs)

-- prod [] = 1
-- prod (x:xs) = x * (prod xs)

-- and [] = True
-- and (x:xs) = x && (and xs)

-- lop [] = v
-- lop (x:xs) = x op (lop xs)
-- sum [1,2,3] = 1 + (2 + (3 + 0))

let foldr f acc [] = acc
    foldr f acc (x:xs) = x `f` (foldr r acc xs)

let foldl f acc [] = acc
    foldl f acc (x:xs) = let acc' = acc `f` x
                         in foldl acc' xs
-- foldl (op) v [x,y,z] = ((v op x) op y) op z
let sum'' = foldl (+) 0
let reverse' = foldl (flip (:)) []
reverse' [1,2,3,4]

scanl (+) 0 [1,2,3,4,5]
scanr (+) 0 [1,2,3,4,5]

:t ($)
sum (filter (>10) (map (*2) [1..10]))
sum $ filter (>10) $ map (*2) [1..10]
map ($5) [(5+), (5*), (^2)]

:t (.) -- to apply function composition, it has higher precedence of the $ but lower to normal applications
sum (filter odd (map (^2) [1..100]))
sum . filter odd . map (^2) $ [1..100]

let reverseWords = unwords . map reverse . words
data Shape = Circle Double | Rectangle Double Double deriving (Show)
:t Circle
let surface :: Shape -> Double
    surface (Circle r) = pi * r^2
    surface (Rectangle l h) = l*h

surface $ Circle 5
data Point = Point Double Double deriving (Show)
data Shape = Circle Point Double | Rectangle Point Point deriving (Show)
let surface :: Shape -> Double
    surface (Circle _ r) = pi * r^2
    surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs(x1-x2)*abs(y1-y2)

surface $ Rectangle (Point 0 0) $ Point 3 4

data Person = Person String String String deriving (Show)
country :: Person -> String
firstname :: Person -> String
lastname :: Person -> String
Person "Mario" "Rossi" "Italy"
let guy = Person {firstname = "John", country = "USA"}

data Maybe a = Nothing | Just a deriving (Show)
:t Nothing
let myhead :: [a] -> Maybe a
    myhead [] = Nothing
    myhead (x:xs) = Just x

let (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    v >>= f = case v of
              Nothing -> Nothing
              Just x -> f x

let myDiv10 :: Integer -> Maybe Integer
    myDiv10 0 = Nothing
    myDiv10 n = Just (10 `div` n)

data Day = Mon | Tue | Wed | Thu deriving (Eq, Ord, Show, Read)
