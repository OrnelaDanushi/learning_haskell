-- examples of mathematical notations and the respective Haskell ones 

-- to show the output
main :: IO()

-- f(x) = 2x + 3
f x = 2*x + 3

-- g(x,y) = x^2 + xy + y^2
g (x,y) = x^2 + x*y + y^2

-- abs(x) = x if x>=0, -x oth
abs_ x
  | x>=0      = x
  | otherwise = -x

-- set comprehension: {x | x in X AND f(x)>5}
-- list comprehension (= loops over lists)
list = [1,5,6]
f_list xx = 
  [x | x<-xx, f x > 5]
  -- [x | x<-xx, x > 5]

-- recursion is used in place of loops
power2 n
  | n==0  = 1
  | n>0   = 2*power2(n-1)

main = do
-- if the previous functions are declared inside here then they have to be instatiated with the let clause
print("f application: ", f 2) --7
print("g application: ", g (2,3)) --19
print("abs(-2): ", abs_ (-2)) --2
print("abs on a composition: ", abs_ (f(g(2,3)))) --41
print("filtering on list: ", f_list list)
print("power2 of 3: ", power2 3)
