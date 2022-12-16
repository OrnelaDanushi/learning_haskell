:set +t
:set +m

-- a list is palindrome if it is the same when scanned from left to right and from right to left
-- as noon or civic
-- a function pal that checks if a list is palindrome

let pal [] = True
    pal [x] = True
    pal xs = if head xs == last xs then pal (init (tail xs)) else False

let pal xs = reverse xs == xs

-- if the list is palindrome we must go to check each element and this is expensive
-- can we do less checks? is it necessary to go to check each of them?
-- no is enougth to check half of it
let pal1 xs = half xs == (half . reverse $ xs)
         where half ys = take (length ys `div` 2) ys

-- using pal define a function that takes a list of lists xxs and returns the list of palindrome lists in xss
let pals = filter pal
pals ["civic", "hello", "anna", "desserts stressed"]

-- function select that takes a list of integers and return the list of elements that are followed by its immediate successor
-- as select [1,2,5,7,3,4] must evaluate to [1,3]
let select xs = [x | (x,y) <- zip xs (tail xs), y == succ x]

-- function points that takes a function f :: Int -> Int and the extremes of an interval and returns the list of points (x,f(x))
-- for all the values in the interval
let points f min max = [(x, f x) | x <- [min..max] ]
point succ 1 20

let point f min max = zip [min..max] (map f [min..max])

-- a positive natural number is called perfect if it is equal to the sum of its proper divisors
-- as 6=1+2+3 and 28=1+2+4+7+14, write some code to generate the list of all perfect numbers
let sumproper n = sum [d | d <- [1..(n `div` 2)], n `mod` d == 0]
sumproper 1001
let perfect n = n == sumproper n

let perfects = filter perfect [1..]
take 4 perfects

-- generate the list of fibonacci numbers
let fibs = [fib n | n <- [1..]]
         where fib 1 = 1
               fib 2 = 1
               fib n = fib (n-1) + fib (n-2) -- the number of calls is exponential so it is expensive
take 10 fibs

-- n : m : n+m  : m+(n+m) ...
let fib (n,m) = n : fib (m, n+m)
take 10 $ fib (1,1)

let fibs = 1 : 1 : [ n+m | (n,m) <- zip fibs (tail fibs)]
take 10 $ fib

-- Collat'z chains are built as follows: the chain starts with a positive number
-- if it is 1 we stop
-- if it is even we continue the chain dividing it by 2
-- if it is odd we continue the chain by multiplying it by 3 and adding 1
-- as [3,10,5,16,8,4,2,1] and [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
-- it is conjectured that for all starting numbers the chains finish at the number 1
-- write some code to compute how many Collat'z chains starting with numbers between 1 and 100 have a length greater than 15
let chain 1 = [1]
    chain n | even  n = n:chain (n `div` 2)  
            | odd n = n:chain (3*n+1)
            | otherwise = []
chain 6 -- [6,3,10,5,16,8,4,2,1]

let islong = (>15) . length . chain
islong 6 -- False  

filter islong [1..100]
length $ filter islong [1..100] -- 66

let collength tokens 1 = tokens <= 0
    collength tokens n = (tokens <= 0) || collength (tokens -1) (if (even n) then n `div` 2 else 3*n+1)
collength 15 78 -- True
collength 15 6 -- False

length $ filter (collength 15) [1..100] --66

foldl (\acc i -> if (collength 15 i) then succ acc else acc) 0 [1..100] -- 66
foldr (\i acc -> if (collength 15 i) then succ acc else acc) 0 [1..100] -- 66

-- define a new data structure for representing triangles on a cartesian plane and 2 functions for computing their 
-- perimeter and area
-- hint: given the lengths a,b,c of the sides of the triangle and letting s=(a+b+c)/2 the semiperimeter
-- you can use Heron's formula to compute the area as the square root of s(s-a)(s-b)(s-c)
-- note: Heron's formula as given is numerically unstable for triangles with a very 
-- small angle when using floating point arithmetic
data Point = Point {x :: Double, y :: Double} deriving (Show, Eq)
data Triangle = Triangle {p1 :: Point, p2 :: Point, p3 :: Point} deriving (Show)
let dist (Point x1 y1) (Point x2 y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2
dist (Point 0 0) (Point 3 4) -- 5.0
let sides (Triangle p1 p2 p3) = [dist p1 p2, dist p2 p3, dist p1 p3]
let p1 = Point 0 0
let p2 = Point 0 4
let p3 = Point 3 0
let t = Triangle p1 p2 p3
sides t -- [4.0,5.0,3.0]
let perimeter = sum . sides
perimeter t -- 12.0
let area t = sqrt $ s*(s-a)*(s-b)*(s-c)
           where [a,b,c] = sides t
                 s = (perimeter t)/2
area t -- 6.0

:t undefined -- it belongs to any type, is a sort of bottom or error element at any time, it can rise an excpetion
let dont [] = True
    dont [(x,y)] = True 
    dont (x:xs) = False
dont [undefined] -- exception, this because there is not this case in the definition 
dont [(undefined, undefined)] -- True, so even if we don't know what it is, it is ok since this case of pairs exists

