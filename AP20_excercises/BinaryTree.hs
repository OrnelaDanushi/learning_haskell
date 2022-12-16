module BinaryTree where

--consider the following definition of binary trees:
data IntTree = Leaf Int | Node ( Int, IntTree, IntTree)
--where Leaf and Node are called CONSTRUCTORS

--implement tmap, a "tree version" of the map combinator
--more precisely, the function tmap should take a function f and a tree t, and should apply f to each value in t

--there is no way to print the tree for debugging because show cannot
--print it as a structure, so define an appropriate function to do it, es as a list
showTree (Leaf n) = [(show n)]
showTree (Node (n, t1, t2)) = [(show n)] ++ (showTree t1) ++ (showTree t2) 
--showTree (Node (3, Leaf 4, Node(5, Leaf 6, Leaf 7)))
--["3","4","5","6","7"]

tmap :: (Int -> Int) -> IntTree -> IntTree
tmap f (Leaf n) = Leaf (f n)
tmap f (Node (n, t1, t2)) = Node ((f n), (tmap f t1), (tmap f t2))
--tmap f t = let Node( n, t1, t2) = t in Node ((f n), (tmap f t1), (tmap f t2)))

iD n = n --otherwise id exists already in prelude
-- showTree (tmap (iD) (Leaf 4))
--["4"]
-- showTree (tmap (iD) (Node (3, Leaf 4, Node(5, Leaf 6, Leaf 7))))
--["3","4","5","6","7"]

--in reference to something ike this
--tmap f t = foldl g (0) t 
--g(g ([],l1),  l2)  l3 ... 
--[f l1, f l2, ...]
--  where  g t x = t ++ Node ( Leaf(f x), t, t ) 
--NON è foldable, va prima trasformato in una classe foldable e poi si applica la foldl

--using tmap, implement the function succTree taking a tree t 
--and computing a tree whose elements are the successors of the values in t

sucC n = n + 1 --otherwise succ already exists in prelude
succTree (Leaf n) = Leaf (sucC n)
succTree t = let Node(n, t1, t2) = t in Node ((sucC n), (succTree t1), (succTree t2))
--showTree (succTree (Node (3, Leaf 4, Node(5, Leaf 6, Leaf 7))))
--["4","5","6","7","8"]

--write a function sumSucc taking a tree t
--and computing the sum of the elements of succTree t
sumSucc (Leaf n) = n
sumSucc t = let Node(n, t1, t2) = t in n + (sumSucc t1) + (sumSucc t2)
--sumSucc (Node (3, Leaf 4, Node(5, Leaf 6, Leaf 7)))
--25

--or using reduce
--reduce :: (a -> a -> a) -> Tree a -> a
reduce f (Leaf n) = n 
reduce f (Node  (n,t1,t2)) = 
    let acc1 = (reduce f t1) 
        acc2 = (reduce f t2)
    in acc1  `f` acc2
--reduce (+) (Node(0, Node(0, Leaf 1, Leaf 2),Leaf 3))
-- = (1 + 2) + 3 = 6
