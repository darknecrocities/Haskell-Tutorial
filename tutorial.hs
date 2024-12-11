-- **Haskell Tutorial: From Beginner to Expert**

-- 1. Introduction to Haskell and Variables

-- In Haskell, variables are immutable. Once a value is assigned to a variable, 
-- it cannot be changed. Here, we define a constant value for Pi.

piValue :: Float  -- Declare the type of piValue as Float
piValue = 3.14159 -- Assign a value to piValue

-- The main function to print the value of piValue
main :: IO ()  -- IO action to perform side effects (output)
main = do
    putStrLn ("Pi value is: " ++ show piValue)  -- Concatenate and print Pi value

-- -------------------------------------------------------------

-- 2. Data Types and Type Classes

-- Haskell has several built-in data types like Int, Float, Char, Bool, and String.

myInt :: Int       -- Declare a variable with type Int
myInt = 42         -- Assign the value 42

myFloat :: Float   -- Declare a variable with type Float
myFloat = 3.14     -- Assign the value 3.14

myBool :: Bool     -- Declare a variable with type Bool
myBool = True      -- Assign the value True

myChar :: Char     -- Declare a variable with type Char
myChar = 'A'       -- Assign the character 'A'

myString :: String -- Declare a variable with type String
myString = "Hello, Haskell!" -- Assign a string

-- Type Classes:
-- Type classes are a way of defining operations on data types that share common behavior.

-- Eq: To check for equality
isEqual :: Int -> Int -> Bool  -- Define a function to compare two integers
isEqual a b = a == b           -- Return true if a is equal to b

-- Show: To convert a value to a String
showInt :: Int -> String       -- Define a function that takes an Int and returns a String
showInt x = show x             -- Convert the integer x to a string

-- -------------------------------------------------------------

-- 3. Recursion and Loops

-- In functional programming, recursion is the primary way to perform loops.

-- Basic Recursion: Factorial
factorial :: Int -> Int         -- Define a function to calculate factorial
factorial 0 = 1                -- Base case: factorial of 0 is 1
factorial n = n * factorial (n - 1)  -- Recursive case: n * factorial of (n-1)

-- Main function to print the factorial of 5
main :: IO ()
main = print (factorial 5)  -- Output: 120

-- -------------------------------------------------------------

-- 4. Tail Recursion

-- Tail recursion is a special form of recursion where the recursive call 
-- is the last operation, which allows for more efficient memory usage.

factorialTail :: Int -> Int -> Int   -- Define a tail recursive factorial
factorialTail 0 acc = acc            -- Base case: return the accumulator value
factorialTail n acc = factorialTail (n - 1) (n * acc)  -- Recursive case

-- Main function to print the tail-recursive factorial of 5
main :: IO ()
main = print (factorialTail 5 1)  -- Output: 120

-- -------------------------------------------------------------

-- 5. Object-Oriented Programming Concepts (Simulated in Haskell)

-- While Haskell is not object-oriented, we can simulate object-oriented concepts
-- like classes, inheritance, and polymorphism using data types and type classes.

-- Define a simple "Car" data type with two fields: brand and model
data Car = Car { brand :: String, model :: String } deriving Show

-- Define a type class "Startable" for objects that can "start"
class Startable a where
    start :: a -> String

-- Make the Car type an instance of Startable
instance Startable Car where
    start car = "Starting " ++ brand car ++ " " ++ model car

-- Main function to create a Car and start it
main :: IO ()
main = do
    let myCar = Car "Toyota" "Corolla"  -- Create a new Car
    putStrLn (start myCar)  -- Output: Starting Toyota Corolla

-- -------------------------------------------------------------

-- 6. Data Structures

-- **Linked List**
-- A linked list is a data structure in which each element points to the next one.

data LinkedList a = Empty | Node a (LinkedList a) deriving Show

-- Create a linked list with values 1 -> 2 -> 3
list :: LinkedList Int
list = Node 1 (Node 2 (Node 3 Empty))

-- Function to print the linked list
printList :: Show a => LinkedList a -> String
printList Empty = ""  -- Base case: if the list is empty, return an empty string
printList (Node x rest) = show x ++ " " ++ printList rest  -- Recursive case

-- Main function to print the linked list
main :: IO ()
main = putStrLn (printList list)  -- Output: 1 2 3

-- -------------------------------------------------------------

-- **Binary Tree**
-- A binary tree is a tree data structure where each node has at most two children.

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

-- Example of a binary tree with root 1, left child 2, and right child 3:
tree :: Tree Int
tree = Node 1 (Node 2 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree)

-- -------------------------------------------------------------

-- 7. Advanced Concepts

-- **Monads**
-- Monads are a way of handling side effects in functional programming.
-- A common monad is the "Maybe" monad, which represents a computation that may fail.

-- Define a function that works with Maybe monads
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing  -- Division by zero returns Nothing
safeDivide x y = Just (x `div` y)  -- Otherwise, return the result wrapped in Just

-- Main function to demonstrate the use of Maybe monads
main :: IO ()
main = do
    let result = safeDivide 10 2
    case result of
        Nothing -> putStrLn "Division by zero"
        Just res -> putStrLn ("Result: " ++ show res)  -- Output: Result: 5

-- **Functors**
-- Functors are types that can be mapped over. The most common functor is `Maybe`.

-- Define a function to apply a function over a Maybe value
increment :: Maybe Int -> Maybe Int
increment (Just x) = Just (x + 1)  -- Increment the value inside Just
increment Nothing = Nothing         -- Do nothing if it's Nothing

-- Main function to demonstrate Functors
main :: IO ()
main = do
    let value = Just 5
    print (increment value)  -- Output: Just 6
