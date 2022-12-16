{- 
-- main is a name, putStrLn is a function with a String parameter:
main = putStrLn "hello, world"

-- *Main> :t putStrLn
-- putStrLn :: String -> IO ()
-- takes a String and returns an IO action that has a result type of ()
-- as an empty tuple, also known as unit
-- An IO action is something that, when performed, will carry out an action with a side-effect
-- (as reading from the input or printing stuff to the screen) and will also contain 
-- some kind of return value inside it.
-- In this example, printing a String to the terminal doesn't really have any kind of meaningful
-- return value, so a dummy value of () is used.

-- *Main> :t putStrLn "hello, world"
-- putStrLn "hello, world" :: IO ()
-}

{-
-- An IO action will be performed when we give it a name of main and then run the program
-- But, having a program with just one IO action is limiting. 
-- For this, we can use do syntax (which looks like an imperative program) 
-- to glue together several IO action into one:
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine -- it reads a line from the input and stores it into a variable called name
    putStrLn ("Hey "++ name ++", you rock!")

-- :t main
-- main :: IO ()
-- the type is IO () because that's the type of last IO action inside
-- Because of that, main always has a type signature of main :: IO something
-- where something is some concrete type
-- So, by convention, we don't usually specify a type declaration for main

-- :t getLine
-- getLine :: IO String
-- it is an IO action that contains a result type of String
-- it will wait for the user to input something at the terminal and then that something
-- will be represented as a String
-- So, it performs the IO action and then bind its result value to name
-- Since, it has a type of IO String, name will have a type of String.
-- With the <- construct, we get data out of an IO action
-- getLine is impure because its result value is not guaranteed to be the same when performed twice
-- for this, it is tainted with the IO type constructor and we can only get that data out in IO code
-- and because IO code is tainted too, any computation that depends on tainted IO data will have
-- tainted result.
-- With 'tainted' is meant in not a way that we can never use the result contained in an IO action
-- ever again in pure code.
-- No, we temporarily un-taint the data inside an IO action when we bind it to a name. 
-- so, when we do name <- getLine, name is just a normal String
-}

{-
import Data.Char

tellFortune s = toUpper (head s) : (tail s)

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Read this carefully, because this is your future: "++ tellFortune name

-- tellFortune, or any of the functions it passes name to, doesn't have to know 
-- anything about IO, it's just a String -> String function
-}

{-
nameTag = "Hello, my name is " ++ getLine
-- * Couldn't match expected type `[Char]' with actual type `IO String'
-- * In the second argument of `(++)', namely `getLine'
--   In the expression: "Hello, my name is " ++ getLine
--   In an equation for `nameTag': nameTag = "Hello, my name is " ++ getLine
-- It doesn't work because the ++ constructor requires both its parameters to be
-- lists over the same type, so we cannot concatenate a String and an IO action
-- we first have to get the result out of the IO action to get a value of type String
-- and the only way to do that is to say something like name <- getLine
-- inside some other IO action
-- NB: if we want to deal with impure data, we have to do it in an impure environment
-- and is in our interest to keep the IO parts of our code as small as possible
-}

{-
-- Every IO action that gets performed has a result encapsulated within it.
-- That's why our previous example program could also have been written like this:
main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
-- However, foo would just have a value of (), so doing that would be kind of moot.
-- Notice that we didn't bind the last putStrLn to anything.
-- That's because in a do block, the last action cannot be bound to a name, like the first two were.
-- The do block, automatically, extracts the value from the last action and binds it to its own result.
-- Except for the last line, every line in a do block that doesn't bind can also be written with a bind.
-- So putStrLn "Blah" can be written as _ <- putStrLn "Blah" 
-- but that's useless, so we leave out the <- for IO actions that don't contain
-- an important result, like putStrLn 'something'

-- Beginners sometimes think that doing: name = getLine
-- will read from the input and then bind the value of that to name
-- well, it won't, it gives the getLine IO action a different name called, name
-- NB: to get the value out an IO action, you have to perform it inside another IO action
-- by binding it to a name with <-

-- IO actions will only be performed when they are given a name of main
-- or when they're inside a bigger IO action that we composed with a do block.
-- We can also use a do block to glue together a few IO actions and then
-- we can use that IO action in another do block and so on.
-- Either way, they'll be performed only if they eventually fall into main.
-}

{-
import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- the IO actions in the do block are lined up
-- also the let is lined up with the IO actionsù
-- and the names of the let are lined up with each other?
-- In this case, the result of the map is binded to a name which is used later

-- Then, when to use <- or let bindings?
-- <- is used for performing IO actions and binding their results to names
-- map .. isn't an IO action, it's a pure expression in Haskell
-}

{-
-- the following program continuously reads a line and prints out the same line
-- with the words reversed
-- the program's execution will stop when we input a blank line
main = do 
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

-- it takes a String as "hey you" and then calls words with it to produce a list of words as ["hey","you"]
-- then we map reverse on the list, and put back into one string using unwords
reverseWords :: String -> String
-- with composition
reverseWords = unwords . map reverse . words
-- without composition
-- reverseWords st = unwords ( map reverse (words st) ) 

-- let's take a look to what happens under the else clause
-- because, we have to have exactly one IO action after the else,
-- we use a do block to glue together two IO actions into one
-- main is called recursively and that's okay because main is itself an IO action
-- so, we go back to the start of the program

-- the return in Haskell is really nothing like the return in most other languages!
-- it has the same name, which confuses a lot, but in reality it's quite different
-- in ILs, it usually ends the execution of a method and makes it report some sort of value 
-- to whoever called it.
-- in Haskell, in IO actions specifically, it makes an IO action out of a pure value.
-}

{-
-- but using return doesn't cause the IO do block to end in execution or anything like that
-- for instance, this program will quite happily carry out all the way to the last line:
main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "Bau bau"
    return 4
    putStrLn line

-- all these returns do is that they make IO actions that don't really do anything
-- except have an encapsulated result and that result is thrown away because
-- it isn't bound to a name
-}

{-
-- we can use return in combination with <- to bind stuff to names
main = do
    a <- return "hell"
    b <- return "yeah"
    putStrLn $ a ++ " " ++ b

-- return is sort of the opposite to <- 
-- while return takes a value and wraps it up in a box, 
-- <- takes a box, and performs it, and takes the value out of it, binding it to a name
-}

-- since this is redundant, can be used let bindings in do blocks to bind to names
main = do 
    let a = "hell"
        b = "yeah"
    putStrLn $ a ++ " " ++ b

-- NB: when dealing with IO do blocks, we mostly use return 
-- either because we need to create an IO action that doesn't do anything 
-- or because we don't want the IO action that's made up from a do block 
-- to have the result value of its last action,
-- but we want it to have a different result value, so we use return to make 
-- an IO action that always has our desired result contained and 
-- we put it at the end