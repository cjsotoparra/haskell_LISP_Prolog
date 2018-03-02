-- ==========================================================================================================
-- Author: Chrisitan Soto
-- Prof.: Christian Colberg
-- Program name: funs6.hs
-- Discription: The point of this script to use build functions that use high order functions and to build
--              functions that work with user defined data types. 
-- ==========================================================================================================

import Data.List

-- =========================================================================================================
-- Function name: max'
-- Parameters: Takes in a list [a] and returns the max element of the list a 
-- Example: max' [2,3,4,5,1,2] == 5 
-- ========================================================================================================= 

max' :: (Ord a) => [a] -> a
max' = head . reverse . sort  


-- ==========================================================================================================
-- Function name: mul'
-- Parameters: Takes in a list [a], an elemtent a,  and returns a list where each element was multiplied by a
-- Example: mul' [1,2,3,4,5] 0.0  == [0.0,0.0,0.0,0.0,0.0,]
-- ==========================================================================================================

mul' :: (Num a) => [a] -> a -> [a]
mul' xs x = map (*x) xs


-- ==========================================================================================================
-- Function name: sumc
-- Parameters: [Ints]
-- Output: Int
-- Discription: sumc takes a list of Ints and cubes the elements, filters where numbers are divisible by and 
--              then takes the sum of the list
-- Example: sumc [7] == 343
-- ==========================================================================================================

sumc :: [Int] -> Int
sumc xs = foldr (+) 0 (map cube (filter by7 xs)) 
   where cube x = x*x*x
         by7 x | x `mod` 7 == 0 = True
               | otherwise = False

-- ==========================================================================================================
-- Function name: siever
-- Parameters: Int -> [Int]
-- Output: [Int]
-- Discription: siever takes an Int and and a list of Ints [Int] and filters the list and returns elements
--              that are not multiples of Int
-- Example: seiver 2 [2..10] == [3,5,7,9]
-- ==========================================================================================================

siever :: Int -> [Int] -> [Int]
siever n xs = filter  byn xs
         where byn x | x `mod` n == 0 = False
                     | otherwise = True

-- =================================================================
-- Comparisons
-- =================================================================
intCMP :: Int -> Int -> Ordering
intCMP a b | a == b = EQ 
           | a < b  = LT
           | otherwise = GT

strCMP :: String -> String -> Ordering
strCMP a b | a == b = EQ 
           | a < b  = LT
           | otherwise = GT

doubleCMPRev :: Double -> Double -> Ordering
doubleCMPRev a b | a == b    = EQ 
                 | a < b     = GT
                 | otherwise = LT

-- =================================================================
-- Operations on sets
-- =================================================================
isSet  :: Ord a => (a -> a-> Ordering) -> [a] -> Bool
isSet cmp xs =  and (map (isSetHelper cmp) (zip xs (tail xs)))

-----------------------------------------------------------------
-- Return true if x occurs in xs using cmp as the comparison function
member :: Ord a => (a -> a-> Ordering) -> a -> [a] -> Bool
member cmp x xs = or (map (memberHelper cmp x) xs)

-----------------------------------------------------------------
-- Make a set from a list by removing duplicates and sorting
makeSet :: Ord a => (a -> a-> Ordering) -> [a] -> [a]
makeSet cmp xs =  map (head) (dupList (sort xs)) 
-----------------------------------------------------------------
-- Return the set intersection of two sets (previously constructed
-- by makeSet), using cmp as the element comparison function.
setIntersect  :: Ord a => (a -> a-> Ordering) -> [a] -> [a] -> [a]
setIntersect cmp xs ys = [x | x <- (makeSet cmp xs), y <- (makeSet cmp ys), cmp x y == EQ]

-----------------------------------------------------------------
-- Return the set union of two sets (previously constructed
-- by makeSet), using cmp as the element comparison function.
setUnion  :: Ord a => (a -> a-> Ordering) -> [a] -> [a] -> [a]
setUnion cmp xs ys = makeSet cmp (sort((makeSet cmp xs) ++ (makeSet cmp ys)))

-- Return the set subtraction of two sets (previously constructed
-- by makeSet), using cmp as the element comparison function.
setSubtract  :: Ord a => (a -> a-> Ordering) -> [a] -> [a] -> [a]
setSubtract cmp xs ys = [x | x <- (makeSet cmp xs), filter (==x) ys == []]

-- Return True or False if set xs of set ys (previously constructed
-- by makeSet), using cmp as the element comparison function.
setIsSubset  :: Ord a => (a -> a-> Ordering) -> [a] -> [a] -> Bool
setIsSubset cmp xs ys = length (makeSet cmp xs) == length (mySubsetTuple cmp (makeSet cmp xs) (makeSet cmp ys))

-----------------------------------------------------------------
-- Compute the similarity (a value between 0.0 and 1.0) between two
-- sets. If both sets are empty, return 0.0.
setSimilarity :: Ord a => (a -> a-> Ordering) -> [a] -> [a] -> Double
setSimilarity cmp [] [] = 0.0
setSimilarity cmp xs ys = (fromIntegral (length(setIntersect cmp xs ys))) / (fromIntegral(length (setUnion cmp xs ys)))

data Nat = 
  Zero |
  Succ Nat
  deriving Show

data IntList = 
    IntCons Int IntList | 
    IntNil
    deriving Show

sum' :: Num t => IntList -> Int
sum' IntNil = 0
sum' (IntCons x xs) = x + (sum' xs)

  
-- ==========================================================================================================
-- Function name: toPeano
-- Parameters: Int 
-- Output: Nat
-- Discription: toPeano takes in an int and returns an Nat element of the size of the int in the parameter
-- Example: toPeano 0 == Zero
-- ==========================================================================================================

toPeano :: Int -> Nat
toPeano 0 = Zero
toPeano x = Succ ( toPeano (x-1))

-- ==========================================================================================================
-- Function name: fromPeano
-- Parameters: Nat 
-- Output: Int
-- Discription: This function takes in a Nat and returns the size of the Nat
-- Example: fromPeano (Succ Zero) == 1
-- ==========================================================================================================

fromPeano :: Nat -> Int
fromPeano Zero = 0
fromPeano (Succ Zero) = 1 
fromPeano (Succ n) = 1 + fromPeano (n)

-- ==========================================================================================================
-- Function name: addPeano
-- Parameters: Nat -> Nat
-- Output: Nat
-- Discription: This function takes in 2 Nats as parameters and returns a Nat the size of the 2 Nats in the
--              the parameter
-- Example: addPeano (Succ Zero) (Succ Zero) = Succ (Succ Zero))
-- ==========================================================================================================

addPeano :: Nat -> Nat -> Nat
addPeano Zero b = b
addPeano a Zero = a
addPeano a (Succ b) = addPeano (Succ a) b

-- ==========================================================================================================
-- Function name: mulPeano
-- Parameters: Nat, Nat
-- Output: Nat
-- Discription: This function takes in 2 Nats and returns a Nat the size of the 2 Nats multiplied
-- Example: mulPeano (Succ (Succ Zero)) (Succ (Succ Zero)) == Succ (Succ (Succ (Succ Zero)))
-- ==========================================================================================================

mulPeano :: Nat -> Nat -> Nat
mulPeano a Zero = Zero
mulPeano Zero b = Zero
mulPeano (Succ Zero) b = b
mulPeano a (Succ Zero) = a
mulPeano a (Succ b) = (addPeano a (mulPeano a b))

-- ==========================================================================================================
-- Function name: peanoEQ
-- Parameters: Nat, Nat
-- Output: Bool
-- Discription: This function takes in 2 Nats and returns True if they are equal, False otherwise
-- Example: peanoEQ (Succ Zero) (Zero) == False
-- ==========================================================================================================

peanoEQ :: Nat -> Nat -> Bool
peanoEQ Zero Zero = True
peanoEQ Zero b = False
peanoEQ a Zero  = False
peanoEQ (Succ a) (Succ b) = peanoEQ a b

-- ==========================================================================================================
-- Function name: peanoLT
-- Parameters: Nat, Nat
-- Output: Bool
-- Discription: This function takes in 2 Nat and returns True if the first parameter Nat is less than the
--              second parameter Nat
-- Example: peanoEQ (Succ Zero) (Succ (Succ Zero)) == True
-- ==========================================================================================================

peanoLT :: Nat -> Nat -> Bool
peanoLT Zero Zero = False
peanoLT Zero b = True
peanoLT a Zero = False
peanoLT (Succ a) (Succ b) = peanoLT a b

-- =================================================================
-- Auxiarlary functions
-- =================================================================

-- ==========================================================================================================
-- Function name: memberHelper
-- Parameters: (a -> a -> Ordering) -> a -> a
-- Output: Bool
-- Discription: This function takes in 2 elements and simply compares if they are equals and returns True,
--              otherwise it returns False
-- Example: memberHelper intCMP 1 2 == True
-- ==========================================================================================================

memberHelper :: (a -> a -> Ordering) -> a -> a -> Bool
memberHelper cmp a b    | cmp a b == EQ = True
                        | otherwise = False

-- ==========================================================================================================
-- Function name: isSetHelper
-- Parameters: (t -> t1 -> Ordering) -> (t, t1)
-- Output: Bool
-- Discription: this function compares a set of tuple and returns True if t is less than t1, otherwise it 
--              it returns False
-- Example: isSetHelper intCMP (1,2) == True
-- ==========================================================================================================


isSetHelper :: (t -> t1 -> Ordering) -> (t, t1) -> Bool
isSetHelper cmp (a, b) | cmp a b == LT = True
                       | otherwise = False

-- ==========================================================================================================
-- Function name: mySubsetTuple
-- Parameters: Ord a => (a -> a -> Ordering) -> [a] -> [a]
-- Output: [(a,a)]
-- Discription: This function takes in 2 lists and returns a list of tuples with the restriction of x == y
-- Example: mySubsetTuple intCMP [1,2] [1,2,3,4] == [(1,1),(2,2)]
-- ==========================================================================================================

mySubsetTuple :: Ord a => (a -> a -> Ordering) -> [a] -> [a] -> [(a,a)]
mySubsetTuple cmp xs ys = [(x,y) | x <- xs, y <- ys, x == y]

-- ==========================================================================================================
-- Function name: dupList
-- Parameters: (Eq a) => [a]
-- Output: [[a]]
-- Discription: The purpose of this function is to take a list and group elements together into a list and 
--              then puts that list into a list.
-- Example: dupList [1,1,2,2,3,3] == [[1,1],[2,2],[3,3]]
-- ==========================================================================================================

dupList :: (Eq a) => [a] -> [[a]]
dupList []     = []
dupList (x:xs) = (x : takeWhile (== x) xs) : dupList (dropWhile (== x) xs)

-- ==========================================================================================================
-- Function name: len
-- Parameters: (Eq a) => [a]
-- Output: [[a]]
-- Discription: The purpose of this function is to take a list and group elements together into a list and
--              then puts that list into a list.
-- Example: dupList [1,1,2,2,3,3] == [[1,1],[2,2],[3,3]]
-- ==========================================================================================================
len = foldr (\_ n -> 1 + n) 0

-- =================================================================
-- List Comprehension Lists and Matrix
-- =================================================================

-- ==========================================================================================================
-- Function name: row
-- Parameters: Int, Int
-- Output: [Int]
-- Discription: The purpose of row is to take in 2 Ints and return a list of 0's and 1 at the index in the 
--              second parameter.
-- Example: row 5 2 == [0,1,0,0,0]
-- ==========================================================================================================

row :: Int -> Int -> [Int]
row n v = [if z == v then 1 else 0 | z <- [1..n]]

-- ==========================================================================================================
-- Function name: idmatrix
-- Parameters: Int
-- Output: [[Int]]
-- Discription: The purpose of this function is to return a square identity matrix of the size of the first
--              parameter
-- Example: idmatrix 2 == [[1,0],[0,1]]
-- ==========================================================================================================

idmatrix :: Int -> [[Int]]
idmatrix 0 = []
idmatrix x = [row x v | v <- [1..x]]

