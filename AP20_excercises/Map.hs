--recall the higher-order combinator map from the prelude
--map :: (a -> b) -> [a] -> [b]

--implement map using the combinator foldl
--foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
--map_1 :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]

map_1 f l = foldl g  []  l 
--g(g ([],l1),  l2)  l3 ... 
--[f l1, f l2, ...]
  where  g l x = l ++  [f x]
  
--  g [] l1 = [ f l1 ]
--  g [f l1 ] l2 = [ f l1, f l2]
  
--  g l x = l ++  [f x]




--use map to take a list of names and return a list of the length of those names
--name_lengths = (map (length) (["Mary", "Isla", "Sam"]))
name_lengths = (map_1 (length) (["Mary", "Isla", "Sam"]))
--print name_lengths returns: [4,4,3]


--use map to square every number in the passed collection
--squares = (map (\x -> x*x) ([1,2,3,4]))
squares = (map_1 (\x -> x*x) ([1,2,3,4]))
--print squares returns: [1,4,9,16]