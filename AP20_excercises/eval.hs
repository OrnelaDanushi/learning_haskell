--import BinaryTree --il nome deve essere lo stesso del file su cui viene costruito ma con maiuscolo




-- consider a variant of the following ADT for a simple expression
data Expr a = Const a | Sum (Expr a) (Expr a) | Mul (Expr a) (Expr a) | Div (Expr a) (Expr a) deriving Show

-- define a recursive evaluation function eval for expressions
-- test the function on a couple of simple expressions
--ex: eval(Sum(Mul(Const 2)(Const 3))(Const 4)) which should evaluate to 10
--eval :: Num a => Expr a -> a

eval (Const a) = a 
--eval (Const 3) --3

-- così non va bene perchè non va mai messo il tipo del costruttore nella definizione
-- basta considerarlo in modo generico, poi sarà lui che farà il binding con
-- le varie scelte del pattern matching
--eval (Sum (Expr a) (Expr a)) = (eval (Expr a)) + (eval (Expr a)) 

eval (Sum x y) = (eval x) + (eval y) 
-- eval (Sum x y) = (+) (eval x) (eval y) 
--eval (Sum(Const 2)(Const 3)) --5

eval (Mul x y) = (eval x) * (eval y)
--eval (Mul(Const 2)(Const 3)) --6

------------------------------------------------------------------------------------------

-- enrich the above expressions with a new constructor Div (Expr a) (Expr a)
-- and write an evaluation function safeEval for these extended expressions,
-- interpreting Div as integer division. Test the new function with some expressions.
-- hint: function safeEval must be partial, since division by zero is undefined,
-- and thus it must return a Maybe value

-- così sarebbe sbagliato, facendo Haskell la valutazione lazy,
-- considerebbe come definizione solo l'ultima applicata
-- data Expr a = Expr a | Div (Expr a) (Expr a)

-- con la seguente definizione dobbiamo commentare la precedente
-- altrimenti considera errore la definizione di Const per 2 volte
-- data Expr_ a = Const a | Sum (Expr_ a) (Expr_ a) | Mul (Expr_ a) (Expr_ a) | Div (Expr_ a) (Expr_ a)

-- altrimenti, siccome oltre il nome anche i costruttori interni devono essere diversi
-- li modifichiamo entrambi con l'undescore
data Expr_ a = Const_ a | Sum_ (Expr_ a) (Expr_ a) | Mul_ (Expr_ a) (Expr_ a) | Div_ (Expr_ a) (Expr_ a)

--safeEval :: (Eq a, Fractional a) => Expr_ a -> Maybe a

--safeEval (Const_ a) = if (a==0) then Nothing else Just a --a
-- is WRONG to get Nothing for evaluating to 0 for other expressions, so this must be changed

safeEval (Const_ a) = Just a

-- così è sbagliato perchè la x potrebbe essere una Div che non è definita in eval con la stuttura Expr!!
--safeEval (Sum x y) = eval (Sum x y) 
--safeEval (Sum x y) = (safeEval x) + (safeEval y) però non va bene se consideriamo Const_ di tipo Maybe

--safeEval (Sum_ x y) = case (safeEval x) of
--  Nothing -> case (safeEval y) of
--    Nothing -> Just 0
--    Just m -> Just (0 + m)
--  Just l -> case (safeEval y) of
--    Nothing -> Just (0 + 0)
--    Just m -> Just (l + m)
safeEval (Sum_ x y) = case (safeEval x) of
  Nothing -> Nothing -- there is also the case base of Nothing, because x or y could be 
  Just m -> case (safeEval y) of  --the evaluation of a Nothing arised from an illegal division
    Nothing -> Nothing
    Just l -> Just (l+m)
--safeEval (Sum_(Const_ 2)(Const_ 3)) --Just 5
--safeEval (Sum_(Const_ 0)(Const_ 3)) --Just 3
  
--safeEval (Mul_ x y) = case (safeEval x) of
--  Nothing -> case (safeEval y) of
--    Nothing -> Just 0
--    Just m -> Just (0 * m)
--  Just l -> case (safeEval y) of
--    Nothing -> Just (0 * 0)
--    Just m -> Just (l * m)
safeEval (Mul_ x y) = case (safeEval x) of
  Nothing -> Nothing
  Just m -> case (safeEval y) of
    Nothing -> Nothing
    Just l -> Just (l*m)
--safeEval (Mul_(Const_ 2)(Const_ 3)) --Just 6
--safeEval (Mul_(Const_ 0)(Const_ 3)) --Just 0

--safeEval (Div_ x y) = case (safeEval y) of 
--  Nothing -> Nothing
--  Just l -> case (safeEval x) of
--    Nothing -> Just (0 / l)
--    Just m -> Just(l / m)
safeEval (Div_ x y) = case (safeEval y) of
  Nothing -> Nothing
  Just 0 -> Nothing
  Just m -> case (safeEval x) of
    Nothing -> Nothing
    Just l -> Just (l / m)
--safeEval (Div_(Const_ 0)(Const_ 3)) --Just 0.0 
--safeEval (Div_(Const_ 0)(Const_ 0)) --Nothing 
--safeEval (Div_(Const_ 3)(Const_ 0)) --Nothing
--safeEval (Div_(Const_ 3)(Const_ 3)) --Just 1.0


-- helper function used to extract the value from a "Maybe box"
fromJust :: Maybe a -> a
fromJust Nothing = undefined
fromJust (Just x) = x

-- without changing the type of Expr
safeEval' :: Expr Integer -> Maybe Integer
safeEval' (Const a) = Just a
safeEval' (Sum x y) = Just ((+) (fromJust(safeEval' x)) (fromJust(safeEval' y)))
safeEval' (Mul x y) = Just ((*) (fromJust(safeEval' x)) (fromJust(safeEval' y)))
safeEval' (Div x y) =
  let x' = safeEval' x
      y' = safeEval' y 
  in if x' == Just 0
    then Nothing
    else Just ((quot) (fromJust x') (fromJust y'))
------------------------------------------------------------------------------------------

-- define an instance of the constructor class Functor for the expressions of ex 1,
-- in order to be able to fmap over trees(=is the data structure definede by several | which it 
-- is named tree, so is not intended the structure Tree or IntTree).
-- A call to (fmap f e) where
-- e :: Expr a 
-- f :: a -> b
-- should return an expression of type Expr b
-- obtained by replacing all the Const v nodes in e with Const (f v)
-- SOL: also adopting the Expr_ data structure ???

-- in the prelude we have already defined the following class Functor f where
--  fmap :: (a -> b) -> f a -> f b 
-- the program has no error even if we obtain this definition that already exist in the Prelude  
  
--instance Functor [] where 
--  fmap f [] = []
--  fmap f (x:xs) = f x : fmap f xs

--instance Functor Tree where  
--  fmap f (Leaf x) = Leaf (f x)
--  fmap f (Node(t1,t2)) = Node(fmap f t1, fmap f t2)

--instance Functor Maybe where 
--  fmap f (Just s) = Just (f s)
--  fmap f Nothing = Nothing
--these definitions of instances cannot be taken in the program, because already exist in the Prelude

--and can done something similar but with a new definition of the class Functor
--class Expr_Functor f where
--  fexpr_ :: (a -> b) -> f a -> f b

--instance Expr_Functor [] where
--  fexpr_ f [] = []
--  fexpr_ f (x:xs) = (f x):(fexpr_ f xs)

--fexpr_ (\x-> safeEval (Const_ x)) [1,2] --[Just 1.0,Just 2.0]
--fexpr_ (\x-> safeEval (Sum(Const_ 2)(Const_ x))) [1,2] --[Just 3.0,Just 4.0]

--data IntTree = Leaf Int | Node ( Int, IntTree, IntTree)
--instance Expr_Functor IntTree where
--  fexpr_ f (Leaf x) = Leaf (f x)
--  fexpr_ f (Node(x,t1,t2)) = Node((f x), (fexpr_ f t1), (fexpr_ f t2))
--but here we get errors using IntTree because this definition requires less parameters than needed

--what the exersices required was to use the predefined Functor for the new data structure Expr
instance Functor Expr where 
  fmap f (Const a) = Const (f a)
  fmap f (Sum x y) = Sum (fmap f x) (fmap f y)
  fmap f (Mul x y) = Mul (fmap f x) (fmap f y)
  fmap f (Div x y) = Div (fmap f x) (fmap f y)

--fmap (\x -> eval (Const x)) (Const 3) --Const 3
--fmap (id) (Const 3) --Const 3
--fmap (\x -> x+1 ) (Const 3) --Const 4
--fmap (\x -> x+1 ) (Sum(Const 2)(Const 3)) --Sum (Const 3) (Const 4)
--fmap (\x -> x+1 ) eval (Sum(Const 2)(Const 3)) -- 6

------------------------------------------------------------------------------------------

-- TO COMPLETE!!

-- propose a way to define an instance Foldable Expr of the class constructor Foldable,
-- by providing a function to fold values across a tree representing an expression
-- hint: consult Hoogle to discover the 'minimal complete definition' of Foldable
-- several solutions are possible

instance Foldable Expr where
  foldr f acc (Const a) = f a acc
  foldr f acc (Sum x y) = foldr f (foldr f acc x) y
  foldr f acc (Mul x y) = foldr f (foldr f acc x) y

--foldr (+) 0 (Const 3) --3
--foldr (+) 0 (Sum(Const 2)(Const 3)) --5
--foldr (+) 0 (Mul(Const 2)(Const 3)) --5
--foldr (*) 0 (Sum(Const 2)(Const 3)) --0
--foldr (*) 1 (Sum(Const 2)(Const 3)) --6
--foldr (*) 1 (Sum(Const 2)(Const 5)) --10
--foldr (*) 1 (Mul(Const 2)(Const 7)) --14

-- can't be correct: same set, too many operations 
-- so Expr is an instance of Foldable that requires to 
-- implement foldMap not foldr

  foldMap f (Const a) = mempty
  --foldMap f (Sum x y) = (foldMap f x) + (foldMap f y) --BUT THERE ARE ERRORS
  --foldMap f (Mul x y) = foldMap f x * foldMap f y

{-
instance Monoid Expr where
  mempty (Const _) = id
  mempty (Sum _ _) = const 0
  mempty (Mul _ _) = const 1
  mempty (Div _ _) = const 1
  mappend (Const _) = id -- WRONG
  mappend (Sum _ _) = (+)
  mappend (Mul _ _) = (*)
  mappend (Div _ _) = (quot)
-}
------------------------------------------------------------------------------------------

--TO COMPLETE!!

-- consider the following definition of variables for the expressions of ex 1
data Var = X | Y | Z deriving Eq
data Expr' a = Const' a | Sum' (Expr' a) (Expr' a) | Mul' (Expr' a) (Expr' a) | Id Var 

{-
evaluate (Const' a) = Just a 
evaluate (Sum' x y) = case (evaluate x) of
  Nothing -> Nothing
  Just m -> case (evaluate y) of
    Nothing -> Nothing
    Just l -> Just (l+m)
evaluate (Mul' x y) = case (evaluate x) of
  Nothing -> Nothing
  Just l -> case (evaluate y) of
    Nothing -> Nothing
    Just m -> Just (l*m)

--HOW TO EVALUATE THE VARIABLES?? this gives errors
--evaluate (Id v) = Just (id v) --if (a==X || a==Y || a==Z) then (Just a) else Nothing
-}

eval' (Const' a) = a 
eval' (Sum' x y) = (eval' x) + (eval' y) 
eval' (Mul' x y) = (eval' x) * (eval' y)
eval' (Id v) = 0 --this is a false value

--instance Functor Expr' where 
  --fmap v sub (Const' a) = if ( compare v (eval' (Const' a)) )== EQ then (Const' sub) else (Const' a)
  --fmap f (Sum' x y) = Sum' (fmap f x) (fmap f y)
  --fmap f (Mul' x y) = Mul' (fmap f x) (fmap f y)


subVar expr v = elem v expr

-- define a function subst that takes a triple (x,y,z) of expressions,
-- interpreted as the values of X, Y, Z respectively,
-- and an expression and produces a new expression where the variables are substituted
-- with the corresponding expressions

subst (x,y,z) e = (a,b,c)
  where 
    a = eval' x
    b = eval' y
    c = eval' z


-- define the partial function eval, applied to an expression e, 
-- returns its value if e does not contain variables
-- and Nothing otherwhise
--eval' e = if (elem X e) then True else False

-- define function recEval, takes as arguments a triple of expressions (x,y,z) and an expression e,
-- and evaluates e replacing variables with the corresponding expressions when needed
--recval (x,y,z) e = 

------------------------------------------------------------------------------------------

-- write an instance of Show that allow to print expressions (with parenthesis!)

-- using not Expr because otherwise we overlap the Show explicitly derived
-- using not instance Show (Expr' a) where .. because it needs to explicitly
-- state that there exists a Show instance for the type variable a

instance (Show a) => Show (Expr' a) where 
  show (Const' a) = (show a)
  show (Sum' (Const' x) (Const' y)) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Mul' (Const' x) (Const' y)) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
    
--Sum' (Const' 2) (Const' 3)  --(2 + 3)
--Mul' (Const' 2) (Const' 3)  --(2 * 3)
--(Const' 3)  --3

------------------------------------------------------------------------------------------

-- TO COMPLETE!!

-- consider the eval function of ex1 
-- exploiting the IO monad, write two new versions of eval

-- evalPrint, that directly prints the final result of the expression under evaluation

evalPrint :: (Num a, Show a)  => Expr a -> IO ()

evalPrint (Const a) = print a

evalPrint (Sum x y) = do 
  xx <- (evalPrint x) 
  yy <- (evalPrint y) 
  let r = xx + yy --error
  return r
  
--evalPrint (Mul x y) = (eval x) * (eval y)

-- evalPrintSub, that also prints all the intermediate results