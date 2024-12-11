-- **Haskell Advanced Tutorial: Part 2**

-- 1. Type Classes
-- A type class defines a set of functions that can be implemented for various types.
-- It's similar to interfaces in object-oriented languages.

-- Defining a type class for types that can be converted to a string
class ToString a where
    toString :: a -> String

-- Implementing the type class for Int, Bool, and custom data types
instance ToString Int where
    toString x = "Integer: " ++ show x

instance ToString Bool where
    toString True = "True"
    toString False = "False"

instance ToString [a] where
    toString xs = "List of length: " ++ show (length xs)

-- Example usage
printToString :: ToString a => a -> IO ()
printToString x = putStrLn (toString x)

-- 2. Monads
-- Monads are a way to chain computations together, often used for handling side effects (like IO).
-- They can be thought of as a design pattern that simplifies dealing with side effects.

-- The Maybe Monad: used for computations that may fail or return nothing.
data Maybe a = Nothing | Just a deriving Show

-- A function that uses Maybe to avoid errors from division by zero
safeDivide :: Float -> Float -> Maybe Float
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Example usage of the Maybe monad
exampleDivide :: Maybe Float
exampleDivide = safeDivide 10 2

-- The IO Monad: used for managing input/output operations in Haskell.
main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name)

-- 3. Higher-Order Functions
-- Higher-order functions are functions that take other functions as arguments or return them as results.
-- They allow for more abstract, flexible code.

-- Map: Applies a function to each element of a list
increment :: Int -> Int
increment x = x + 1

incrementList :: [Int] -> [Int]
incrementList xs = map increment xs

-- Filter: Selects elements from a list based on a predicate function
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

filterEven :: [Int] -> [Int]
filterEven xs = filter isEven xs

-- Fold: Reduces a list to a single value using a combining function
sumList :: [Int] -> Int
sumList xs = foldl (+) 0 xs

-- 4. Recursion and Tail Recursion
-- Haskell's functional style relies heavily on recursion.

-- A simple recursive function to calculate factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Tail recursion optimization: a more efficient form of recursion
-- Tail recursion ensures that the function does not accumulate extra stack frames.

-- Tail recursive factorial using an accumulator
factorialTail :: Int -> Int
factorialTail n = factorialHelper n 1
    where
        factorialHelper 0 acc = acc
        factorialHelper n acc = factorialHelper (n - 1) (n * acc)

-- 5. Data Structures

-- Linked List: A data structure where each element points to the next one.

data LinkedList a = Empty | Node a (LinkedList a) deriving Show

-- Create a linked list
myList :: LinkedList Int
myList = Node 1 (Node 2 (Node 3 Empty))

-- Function to calculate the length of a linked list
lengthList :: LinkedList a -> Int
lengthList Empty = 0
lengthList (Node _ xs) = 1 + lengthList xs

-- Function to append an element to the linked list
appendList :: a -> LinkedList a -> LinkedList a
appendList x Empty = Node x Empty
appendList x (Node y xs) = Node y (appendList x xs)

-- 6. Lazy Evaluation
-- Haskell is lazily evaluated, meaning it only computes values when they are needed.

-- A large list that is never fully evaluated
infiniteList :: [Int]
infiniteList = [1..]

-- Taking the first 5 elements from the infinite list
firstFive :: [Int]
firstFive = take 5 infiniteList

-- 7. Function Composition
-- Function composition allows you to combine multiple functions into a single function.
-- The composition operator (.) is used for this.

-- Example of function composition:
doubleAndAddFive :: Int -> Int
doubleAndAddFive = (+ 5) . (* 2)

-- Using the composed function:
exampleResult = doubleAndAddFive 3  -- Result: (3 * 2) + 5 = 11

-- **Haskell Advanced Tutorial: Part 2**

-- 1. Type Classes
-- A type class defines a set of functions that can be implemented for various types.
-- It's similar to interfaces in object-oriented programming, but it allows polymorphism in Haskell.
-- For instance, you can define a type class for types that can be converted into a string 
-- and implement it for different types, such as `Int`, `Bool`, and custom data types.

-- Defining a type class for types that can be converted to a string
class ToString a where
    toString :: a -> String

-- Implementing the type class for Int, Bool, and custom data types
instance ToString Int where
    toString x = "Integer: " ++ show x

instance ToString Bool where
    toString True = "True"
    toString False = "False"

instance ToString [a] where
    toString xs = "List of length: " ++ show (length xs)

-- Example usage
printToString :: ToString a => a -> IO ()
printToString x = putStrLn (toString x)

-- 2. Monads
-- Monads are a powerful design pattern for handling computations that involve side effects.
-- They allow you to chain operations together in a way that maintains clean, readable code. 
-- Some common monads are the `Maybe` monad, which is used to handle computations that might fail, 
-- and the `IO` monad, used to handle input and output.

-- The Maybe Monad: used for computations that may fail or return nothing.
data Maybe a = Nothing | Just a deriving Show

-- A function that uses Maybe to avoid errors from division by zero
safeDivide :: Float -> Float -> Maybe Float
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Example usage of the Maybe monad
exampleDivide :: Maybe Float
exampleDivide = safeDivide 10 2

-- The IO Monad: used for managing input/output operations in Haskell.
main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name)

-- 3. Higher-Order Functions
Higher-order functions are functions that take other functions as arguments or return them as results. 
They allow for greater flexibility and abstraction in programming. Common higher-order functions include `map`, `filter`, and `fold`.

-- Map: Applies a function to each element of a list
increment :: Int -> Int
increment x = x + 1

incrementList :: [Int] -> [Int]
incrementList xs = map increment xs

-- Filter: Selects elements from a list based on a predicate.
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

filterEven :: [Int] -> [Int]
filterEven xs = filter isEven xs

-- Fold: Reduces a list to a single value.
sumList :: [Int] -> Int
sumList xs = foldl (+) 0 xs

-- 4. Recursion and Tail Recursion
-- Haskell's functional style relies heavily on recursion, especially for operations like calculating factorials or traversing data structures. 
-- Tail recursion is an optimized form of recursion where the recursive call is the last operation in the function, 
-- which helps avoid stack overflow and improve performance.

-- A simple recursive function to calculate factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Tail recursive factorial using an accumulator
factorialTail :: Int -> Int
factorialTail n = factorialHelper n 1
    where
        factorialHelper 0 acc = acc
        factorialHelper n acc = factorialHelper (n - 1) (n * acc)

-- 5. Data Structures
-- Haskell provides a variety of ways to create and manipulate data structures. Here, we define a linked list and implement functions for calculating its length and appending an element.

-- Linked List: A data structure where each element points to the next one.
data LinkedList a = Empty | Node a (LinkedList a) deriving Show

-- Create a linked list
myList :: LinkedList Int
myList = Node 1 (Node 2 (Node 3 Empty))

-- Function to calculate the length of a linked list
lengthList :: LinkedList a -> Int
lengthList Empty = 0
lengthList (Node _ xs) = 1 + lengthList xs

-- Function to append an element to the linked list
appendList :: a -> LinkedList a -> LinkedList a
appendList x Empty = Node x Empty
appendList x (Node y xs) = Node y (appendList x xs)

-- 6. Lazy Evaluation
-- In Haskell, evaluation is lazy, meaning values are only computed when they are needed. 
-- This allows for the creation of infinite data structures and the ability to work with large datasets efficiently.

-- A large list that is never fully evaluated
infiniteList :: [Int]
infiniteList = [1..]

-- Taking the first 5 elements from the infinite list
firstFive :: [Int]
firstFive = take 5 infiniteList

-- 7. Function Composition
-- Function composition allows you to combine multiple functions into a single function. 
The composition operator (.) is used for this purpose.

-- Example of function composition:
doubleAndAddFive :: Int -> Int
doubleAndAddFive = (+ 5) . (* 2)

-- Using the composed function:
exampleResult = doubleAndAddFive 3  -- Result: (3 * 2) + 5 = 11
