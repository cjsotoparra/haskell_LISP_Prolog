import Data.Char
import Data.List
import System.IO
-- Homework 6: Part B Template -- 

-- ==========================================================================================================
-- Authors: Chrisitan Soto and Zhangpeng Liao 
-- Prof.: Christian Colberg
-- Program name: moss6.hs
-- Discription: The purpose of this script to memic the TurnItIn program to check for plagiarism.
-- ==========================================================================================================

-- =================================================================
-- Utility functions
-- =================================================================

-----------------------------------------------------------------
-- The function call
--   windows size list
-- where
--   size = the size of the window
--   list = a list of elements of arbitrary type
-- will return a list of sublists of 'list',
-- constructed by sweeping a window of size 'size'
-- over list. 
-- For example, if 
--    list = [1,2,3,4,5]
--    size = 2
-- the we return
--    [[1,2],[2,3],[3,4],[4,5]
-- but if 
--    size = 3
-- we return
--    [[1,2,3],[2,3,4],[3,4,5]]

windows :: Int -> [a] -> [[a]]
windows k xs = filter (\x -> length x == k) (map (take k) (myTail xs)) 

-- ==========================================================================================================
-- Function name: myTail
-- Parameters: [a]
-- Example: myTail [1,2,3] == [[1,2,3],[2,3],[3]]
-- ==========================================================================================================

myTail :: [a] -> [[a]]
myTail [] = []
myTail xs = xs : myTail (tail xs)

-----------------------------------------------------------------
-- The function call
--    mapAllPairs f xs
-- where
--    f    = a binary function
--    xs   = a list of elements of arbitrary type
-- returns the result of applying f
--    to all pairs of elemtents drawn from xs.
-- Thus, this function, essentially, computes the
-- cross product of xs with itself, and applies 
-- f to all the resulting pairs of elements. 
-- mapAllPairs :: (a -> a -> b) -> [a] -> [b]
-- mapAllPairs f xs = mapAllPairsHelper f xs 
-- mapAllPairs :: (a -> a -> b) -> [a] -> [b]
-- mapAllPairsHelper f xs = map f (crossProd xs xs)
-- crossProd :: [a] -> [b] -> [(a,b)]
-- crossProd xs ys = [(x,y) | x <- xs, y <- ys]

mapAllPairs :: (a -> a -> b) -> [a] -> [b]
mapAllPairs f xs = [(f x y)| x <- xs, y<- xs]


-- =================================================================
-- Hashes are represented as Ints.
-- =================================================================

type Hash = Int
type HashSet = [Hash]

hash :: String -> Hash
hash x = abs (h x)
   where
      h = foldr (\ x y -> ord x + 3 * y) 1

-- =================================================================
-- A document is represented by its filename (a String).
-- When we've read in a document it is represented as a
-- list of the lines read, plus the line number.
-- =================================================================

-----------------------------------------------------------------
-- The name of a document is it's path, a String.
type DocName = FilePath

-----------------------------------------------------------------
-- A line of text, as read from a file, is represented by
-- a (row-number,line) tuple. The row-number starts at 0.
type Line = (Int,String)

-----------------------------------------------------------------
-- A document, as read from a file, is represented by
-- a list of (row-number,line) tuples.
type Document = [Line]

-- =================================================================
-- Functions for reading text files and splitting them
-- into lists of lines.
-- =================================================================

-----------------------------------------------------------------
-- Given a string with embedded newlines, splitLines
-- will split it into a list of (row-number, line)
-- tuples.

-- ==========================================================================================================
-- Function name: splitLines 
-- Parameters: String
-- Purpose: The prupose of this function is to splet a sting with embeeded newlines and splits into a list of
--          lines
-- Example: splitLines "aaa\nbbbb\nccccc\n" --> [(0,"aaa"),(1,"bbbb"),(2,"ccccc"),(3,"")]
-- ==========================================================================================================

splitLines :: String -> Document
splitLines "" = []
splitLines xs = zip [0..countNewLine xs] (getAllLines xs)

-- ==========================================================================================================
-- Function name: getAllLines
-- Parameters: [Char]
-- Purpose: The purpose of this function is take a string and split at every "\n" character and return a
--          lists of lists
-- Example: getAllLines "aaa\nbbbb\n" -->["aaa","bbbb",""]
-- ==========================================================================================================

getAllLines :: [Char] -> [[Char]]
getAllLines xs = if (last xs) == '\n' then (myspl xs) ++ [""] else (myspl xs)

-- ==========================================================================================================
-- Function name: countNewLine
-- Parameters:  Num t => [Char]
-- Purpose: The purpose of this function is take a string and count the number of newlines 
-- Example: countNewLine "aaa\nbbbb\nccccc\n"
-- ==========================================================================================================

countNewLine :: Num t => [Char] -> t
countNewLine [] = 0
countNewLine xs | head xs == '\n' = 1 + (countNewLine (tail xs))
                | otherwise = countNewLine (tail xs)

-- ==========================================================================================================
-- Function name: splitLinesHelper
-- Parameters:  [Char]
-- Purpose: The purpose of this function is take a string and split the string at the first encounter with a
--          newline and return that string
-- Example: splitLinesHelper "aaa\nbbbb\nccccc\n" --> "aaa"
-- ==========================================================================================================

splitLinesHelper :: [Char] -> [Char]
splitLinesHelper [] = []
splitLinesHelper xs | (head xs) == '\n' = []
                    | otherwise = (head xs) : splitLinesHelper (tail xs)

-- ==========================================================================================================
-- Function name: splitLinesHelper1
-- Parameters:  [Char]
-- Purpose: The purpose of this function is take a string and split the string at the first encounter with a
--          newline and return the tail string
-- Example: splitLinesHelper1 "aaa\nbbbb\n" --> "bbbb\n"
-- ==========================================================================================================

splitLinesHelper1 :: [Char] -> [Char]
splitLinesHelper1 [] = []
splitLinesHelper1 xs | (head xs) == '\n' = tail xs
                     | otherwise = splitLinesHelper1 (tail xs)

-- ==========================================================================================================
-- Function name: myspl
-- Parameters:  [Char]
-- Purpose: The purpose of this function is take a string and split the string at every \n and return a list
--          of lists.
-- Example: myspl ""aaa\nbbbb\n" --> ["aaa","bbbb"]
-- ==========================================================================================================

myspl :: [Char] -> [[Char]]
myspl []  = []
myspl xs = splitLinesHelper xs : myspl (splitLinesHelper1  xs)

-----------------------------------------------------------------
-- Given a list of filenames, loadFiles will read 
-- in the files and return them as a list of 
-- tuples (file-name,file-contents).
loadFiles :: [DocName] -> IO [(DocName,Document)]
loadFiles [] = return []
loadFiles (x:xs) = do 
                cont <- readFile x 
                let lines = splitLines cont
                tail <- loadFiles xs
                let files = (x,lines):tail
                return files

-- =================================================================
-- A Position is the location of a character in a file,
-- represented by the tuple (line-number, column-number).
-- =================================================================
type Position = (Int, Int)

-- =================================================================
-- A shingle is a substring of the original document
-- plus the position at which it occurs. This section
-- contains functions for computing the shingles of
-- a document.
-- =================================================================

type Shingle = (String, Position)

-----------------------------------------------------------------
-- Extract all the singles from a line of text by 
-- sweeping a window of size shingleSize across it.
line2shingles :: Int -> Line -> [Shingle]
line2shingles shingleSize (lineNumber,text) = zip (windows shingleSize text) [(lineNumber, x)| x<-[0.. length (windows shingleSize text)]]

-----------------------------------------------------------------
-- Extract all the shingles from a document
-- by sweeping a window of size windowSize
-- over each of the lines of text.
shingles :: Int -> (DocName,Document) -> (DocName,[Shingle])
shingles shingleSize (doc, xs) = (doc, (foldr (++) [] (map (line2shingles shingleSize) xs)))

-----------------------------------------------------------------
-- Given a list of shingles, convert it to a set of
-- hash values using the function "hash" above. There 
-- can be multiple shingles with the same string value, 
-- mapping to the same hash value, but the output of this 
-- function is a *set*, i.e. an ordered set of unique hashes.
shingles2hashSet :: (DocName,[Shingle]) -> (DocName,HashSet)
shingles2hashSet (doc,xs) = (doc, makeToSet (sort[hash (fst x)|x <- xs ]))

-- ==========================================================================================================
-- Function name: makeToSet
-- Parameters:  [a]
-- Purpose: The purpose of this function is take a list and sorts and removes duplicates 
-- Example: makeToSet [1,2,3,6,5,6] --> [1,2,3,5,6]
-- ==========================================================================================================

makeToSet :: Eq a => [a] -> [a]
makeToSet [] = []
makeToSet (x:xs)| (member x xs) = makeToSet xs
              | otherwise = x: (makeToSet xs)

-- ==========================================================================================================
-- Function name: member
-- Parameters:   Eq t => t -> [t]
-- Purpose: The purpose of this function is take an element and a list of the same type of elements of the 
--          same type and return True if the element is a member otherwise it returns False
-- Example: member 5 [1,2,3,4] --> False
-- ==========================================================================================================

member :: Eq t => t -> [t] -> Bool
member _ [] = False
member y (x:xs) | y == x = True
                | otherwise = member y xs

-- =================================================================
-- This section contains algorithms for winnowing down sets of hashes 
-- to smaller sets. Usually, documents are large, and will result in 
-- way too many hashes being generated. "Winnowing" is any technique for
-- reducing the number of hashes. Here, we give several winnowing
-- functions. They all take a list of shingles as input and return a
-- (usually smaller) list of shingles. Later on, we can use
-- the most appropriate winnowing function depending on our
-- needs.
-- =================================================================
type WinnowFun = (DocName,[Shingle]) -> (DocName,[Shingle])

-----------------------------------------------------------------
-- The identity function, returning the same list of
-- shingles as it is given.
winnowNone :: (DocName,[Shingle]) -> (DocName,[Shingle])
winnowNone (d,xs) = (d,xs)

-----------------------------------------------------------------
-- Return only those shingles whose hash values is "0"
-- mod p.

-- ================================================================================================================================
-- Function name: winnowModP
-- Parameters:    Int -> (DocName,[Shingle])
-- Purpose: The purpose of this function is to take a DocName and shingles and returns the DocName and those
--          shingles whose hash values are 0 mod p
-- Example: winnowModP 5 ("fred,[("yab",(0,0)),("abb",(0,1)),("ada",(1,0)),("dab",(1,1)),("abb",(1,2)),("bba",(1,3)),("doo",(3,0))])
-- =================================================================================================================================

winnowModP :: Int -> (DocName,[Shingle]) -> (DocName,[Shingle])
winnowModP p (d,xs) = (d, (filter (modP p) xs))

-- ==========================================================================================================
-- Function name: modP
-- Parameters: Hash -> (String, b)
-- Purpose: The purpose of this function is take a hash and tuple of a string and element and True if 
--          String mod p == 0 otherwise it returns False
-- ==========================================================================================================

modP :: Hash -> (String, b) -> Bool
modP p x = ((hash (fst x)) `mod` p) == 0

-----------------------------------------------------------------
-- Sweep a window of size p across the list of shingles.
-- For every window only keep the one with the minimum value.
-- If there are more than one 
winnowWindow :: Int -> (DocName,[Shingle]) -> (DocName,[Shingle])
winnowWindow p (d,xs) = (d,xs)

-- =================================================================
-- Main moss algorithm for computing the similarities between
-- n different documents.
-- =================================================================

-----------------------------------------------------------------
-- Given a list of documents (tuples of (docName, hash-set)),
-- compare all pairs of documents for similarity. This function 
-- returns a sorted list of tuples (DocName1,DocName2,SimilarityScore).
-- SimilarityScore is the value returned by the setSimilarity 
-- function over the hash-sets of each pair of documents. The 
-- resulting list is sorted, most similar documents first.
compareAllDocs :: [(DocName,HashSet)] -> [(DocName,DocName,Double)]
compareAllDocs xs = sortBy sortF (mapAllPairs (\ x y -> ((fst x),(fst y),(setSimilarity intCMP (snd x) (snd y)) )) xs)

-- =================================================================
-- Comparisons
-- =================================================================
intCMP :: Int -> Int -> Ordering
intCMP a b | a == b = EQ
           | a < b = LT
           | otherwise = GT

intCMPRev :: Int -> Int -> Ordering
intCMPRev a b | a == b = EQ
              | a < b = GT
              | otherwise = LT

floatCMP :: Float -> Float -> Ordering
floatCMP a b | a == b = EQ
             | a < b = LT
             | otherwise = GT

addIt :: Eq a => a -> [a] -> [a]
addIt x xs | elem x xs = xs
           | otherwise = x:xs

--------------------------------------------------------------------------
-- Return the set intersection of two sets (previously constructed
-- by makeSet), using cmp as the element comparison function.
setIntersect :: Ord a => (a -> a -> Ordering) -> [a] -> [a] -> [a]
setIntersect cmp xs ys = filter (\ x -> elem x xs) ys

--------------------------------------------------------------------------
-- Return the set union of two sets (previously constructed
-- by makeSet), using cmp as the element comparison function.
setUnion :: Ord a => (a -> a -> Ordering) -> [a] -> [a] ->[a]
setUnion cmp xs ys = foldr addIt xs ys

--------------------------------------------------------------------------
-- Compute the similarity (a value between 0.0 and 1.0) between two
-- sets. If both sets are empty, return 0.0.
setSimilarity :: Ord a => (a -> a -> Ordering) -> [a] -> [a] ->Double
setSimilarity _ [] [] = 0.0
setSimilarity cmp p q = (fromIntegral (length (setIntersect cmp p q))) / (fromIntegral(length (setUnion cmp p q)))


sortF :: (Ord a1, Ord a) => (a1, t, a) -> (a1, t1, a) -> Ordering
sortF (a1,b1,c1) (a2,b2,c2)
   | c1 < c2 = GT
   | c1 > c2 = LT
   | c1 == c2 = (sortF1 a1 a2)

sortF1 :: Ord a => a -> a -> Ordering
sortF1 a1 a2
   | a1 < a2 = LT
   | a1 > a2 = GT
   | a1 == a2 = EQ

-----------------------------------------------------------------
-- The function call
--    kgram windowSize winnow (name,doc)
-- where
--   windowSize = the size of the window which we will sweep
--                across the document.
--   winnow     = the function which winnows down lists of
--                shingles to a manageable size
--   (name,doc) = the name and the contents of the file we
--                want to process.
-- performs the following steps:
--   1) compute the list of shingles of size windowSize
--   2) winnow down the list of shingles to a manageable size
--   3) convert the list of shingles to a set of hashes.
kgram :: Int -> WinnowFun -> (DocName,Document) -> (DocName,HashSet)
kgram windowSize winnow = shingles2hashSet.(winnow).(shingles windowSize)

-----------------------------------------------------------------
-- The function call
--   moss windowSize winnowFunction listOfDocuments
-- where
--    windowSize      = the size of the windows we sweek across the document
--    winnowFun       = the function that winnows down the large 
--                      list of shingles to a more manageable list
--    listOfDocuments = a list of pairs (document-name, document-contents)
--                      that we want to compare
-- performs the following steps:
--    1) use 'kgram' to compute a set of hashes for each document
--    2) use 'compareAllDocs' to compute, for every pair of documents,
--       how similar they are.
-- This function uses the compareAllDocs function above
-- to compute a list of all pairs of documents and a
-- measure of how similar they are.
moss :: Int -> WinnowFun ->[(DocName,Document)] -> [(DocName,DocName,Double)]
moss windowSize winnowFun = compareAllDocs.(map (kgram windowSize winnowFun))

-----------------------------------------------------------------
-- The function call
--    similar shingleSize winnowAlg fileNames
-- performs the actions
--    1) read in all the files in fileNames (a list of strings),
--    2) compute the simiarity of each pair of documents.
-- This function uses compareAllDocs above to do the actual
-- comparisons.
similar :: Int -> WinnowFun -> [DocName] -> IO [(DocName, DocName, Double)]
similar shingleSize winnowAlg fileNames = do
      docs <- loadFiles fileNames
      let w = moss shingleSize winnowAlg docs
      return w

-----------------------------------------------------------------
-- Below are four functions which are all specializations
-- of the "similar" function.
--    1) sim_none does no winnowing.
--    2) sim_modP uses the mod P winnowing algorithm.
--    3) sim_window uses the window-based winnowing algorithm
--    4) sim is like sim_modP but with a default shingle size
--       set to 3 and with winnowing set to mod 4.
sim_none :: Int -> [DocName] ->  IO [(DocName, DocName, Double)]
sim_none shingleSize = similar shingleSize winnowNone 

sim_modP :: Int -> Int -> [DocName] -> IO [(DocName, DocName, Double)]
sim_modP shingleSize modP = similar shingleSize (winnowModP modP) 

sim_window :: Int -> Int -> [DocName] -> IO [(DocName, DocName, Double)]
sim_window shingleSize windowSize = similar shingleSize (winnowWindow windowSize) 

sim :: [DocName] -> IO [(DocName, DocName, Double)]
sim files = sim_modP 3 4 files 

