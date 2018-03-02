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

-- =================================================================
-- PART C FUNCTIONS 
-- =================================================================

-----------------------------------------------------------------
-- This is the main function for visualizing the
-- similarities between two documents.
-- The functio  call
--   visualize doc1name doc2name
-- where 
--    doc1name = the name of the first document
--    doc2name = the name of the second document
-- this function will
--    1) read in the files,
--    2) compute their similarities (by comparing the set of shingles),
--    3) compute an html string visualizing the similarities,
--    4) write the html string to the file named
--           doc1name-doc2name.html.
visualize :: Int -> DocName -> DocName -> IO()
visualize shingleSize doc1name doc2name = do
          let outfileName = doc1name ++ "-" ++ doc2name ++ ".html"
          [doc1,doc2] <- loadFiles [doc1name,doc2name] 
          let shingles1 = shingles shingleSize doc1
          let shingles2 = shingles shingleSize doc2
          let common = commonStrings shingles1 shingles2
          let regions1 = shingles2regions shingles1 common
          let regions2 = shingles2regions shingles2 common
          let html = showSimilarities doc1 regions1 doc2 regions2
          writeFile outfileName html

-----------------------------------------------------------------
-- The viz function is a specializion of "visualize" with
-- the shingle size set to 3.
viz :: DocName -> DocName -> IO()
viz = visualize 3

-----------------------------------------------------------------
-- A Region is a tuple (pos,len) that represents 
-- a segment of a file starting in position pos
-- (a (row,column) tuple) and extending for len
-- characters. For simplicity, a Region never
-- extends past the end of a line.
type Region = (Position,Int)

-----------------------------------------------------------------

-- The function call
--   inRegion pos region
-- returns True if pos is within region.
-- Regions don't span lines.
inRegion :: Position -> Region -> Bool
inRegion (row,col) ((r,c),len) = row==r && col>=c && col<(c+len)

-- The function call
--   inRegions pos listOfRegions
-- returns True if pos is within any of the
-- regions in listOfRegions. 
inRegions :: Position -> [Region] -> Bool
inRegions pos regions = length(filter (\x -> (inRegion pos x)) regions) >= 1

-----------------------------------------------------------------
-- The function call
--   commonStrings (doc1name,shingles1) (doc2name,shingles2)
-- where
--   doc1name  = name of the first document
--   shingles1 = the list of shingles of the first document
--   doc2name  = name of the second document
--   shingles2 = the list of shingles of the second document
-- returns
--   the *set* of the shingle strings that occur in both documents
--   (i.e. compute the *intersection* of the shingle strings in the
--   two documents).
commonStrings :: (DocName,[Shingle]) -> (DocName,[Shingle]) -> [String]
commonStrings (_,shingles1) (_,shingles2) = makeToSet (commonS shingles1 shingles2)

getStringFromShingles xs = foldr (\x y -> (fst x):y) [] xs
commonS s1 s2= filter (\x -> elem x (getStringFromShingles s1)) (getStringFromShingles s2)

-----------------------------------------------------------------
-- The function call 
--   shingles2regions (doc,shingles) common
-- where
--   doc      = the name of a document
--   shingles = the list of the shingles of the document
--   common   = a list of strings that occur in this document
--              (and which, eventually, we will want to highlight
--              because they also occur in the document we're
--              comparing too)
-- returns 
--   a list of regions where the strings in common 
--   occur in shingles.
shingles2regions :: (DocName,[Shingle]) -> [String] -> [Region]
shingles2regions (doc,shingles) common = sortBy sortF2 (map (\x -> ((snd x),length(fst x))) (foldr (++) [] (getAllShinglesInRegions shingles common)))

getAllShinglesInRegions :: Eq a => [(a, b)] -> [a] -> [[(a, b)]]
getAllShinglesInRegions s common = map (findAllRegion s) common

findAllRegion :: Eq a => [(a, b)] -> a -> [(a, b)]
findAllRegion s c = filter (\x -> (fst x)==c) s

sortF2 :: Ord a => ((a,a),a) -> ((a,a),a) -> Ordering
sortF2 ((a1,b1),c1) ((a2,b2),c2)
   | b1 < b2 = LT
   | b1 > b2 = GT
   | b1 == b2 = EQ

-----------------------------------------------------------------
-- The function call
--   showSimilarities (doc1name,doc1) reg1 (doc2name,doc2) reg2
-- where
--    doc1name    = name of the first document
--    doc1        = the text of document 1 as a list of lines
--    reg1        = a list of regions of document 1 that should 
--                  be highlighted
--    doc2name    = name of the second document
--    doc2        = the text of document 2 as a list of lines
--    reg2        = a list of regions of document 2 that should 
--                  be highlighted
-- will perform the actions
--    1) compute a version of doc1 where regions in reg1 
--       have been highlighted
--    2) compute a version of doc2 where regions in reg2 
--       have been highlighted
--    3) construct html code that displays the hightlighted 
--       documents side by side
showSimilarities :: (DocName,Document) -> [Region] -> (DocName,Document) -> [Region] -> String
showSimilarities (doc1name,doc1) reg1 (doc2name,doc2) reg2 = html
  where
     ann1 = highlight doc1 reg1
     ann2 = highlight doc2 reg2
     html = htmlTemplate (doc1name,ann1) (doc2name,ann2)

-----------------------------------------------------------------
-- The function call
--   highlight doc regions 
-- where 
--   doc     = a text document represented as (row,line) pairs
--   regions = a list of ((row,column),length) tuples, 
--             representing the regions of the document that
--             should be highlighted
-- will return a string where each character c whose position
-- lies within any of the regions has been highlighted (i.e.
-- replaced by the output of the
--       highlightChar c
-- function call).
highlight :: Document -> [Region] -> [String]
highlight [] _ = []
highlight (x:xs) ys = (foldr (++) [] (getAllString x (sortBy sortF2(getCurrentLineRegion x ys)) 0)) ++ (highlight xs ys)

getCurrentLineRegion :: Eq a => (a, b2) -> [((a, b), b1)] -> [((a, b), b1)]
getCurrentLineRegion x ys = filter (\y -> (fst (fst y))==(fst x)) ys    
getAllString x ys n | n == (length(snd x)) = []
                    | inRegions ((fst x),n) ((head ys):[]) = (getRegionStrings (snd x) n (snd (head ys))) : (getAllString x (tail ys) (n + (snd (head ys))))
                    | otherwise = (addStrings (snd x) n (snd (fst (head ys))):[]) : (getAllString x ys  (snd (fst (head ys))))

getRegionStrings :: (Num t, Eq t) => [Char] -> Int -> t -> [String]
getRegionStrings str start length | length == 0 = []
                                  | otherwise = (highlightChar (str!!start)) : (getRegionStrings str (start + 1) (length - 1))

addStrings :: [t] -> Int -> Int -> [t]
addStrings str start end | end == start = []
                         | otherwise = (str!!start) : (addStrings str (start + 1) end)
-----------------------------------------------------------------
-- This function wraps a string in html that
-- gives it a yellow background color.
highlightChar :: Char -> String
highlightChar x = "<FONT style=\"BACKGROUND-COLOR: yellow\">" ++ [x] ++ "</FONT>"

-----------------------------------------------------------------
-- The function call
--   htmlTemplate (doc1name lines1) (doc2name lines2)
-- returns a string containing html that
-- displays lines1 and lines2 side by side.
htmlTemplate :: (DocName,[String]) -> (DocName,[String]) -> String
htmlTemplate (doc1name,lines1) (doc2name,lines2)= "\
   \<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n\
   \<html>\n\
   \  <head>\n\
   \    <title>" 
   ++ 
   title 
   ++ 
   "</title>\n\
   \  </head>\n\
   \\n\
   \  <body>\n\
   \<center>\n\
   \    <h1>"
   ++
   title
   ++
   "</h1>\n\
   \   <table border=1>\n\
   \      <tr>\n\
   \         <th>" ++ doc1name ++ "</th>\n\
   \         <th>" ++ doc2name ++ "</th>\n\
   \      </tr>\n\
   \      <tr>\n\
   \         <td>\n\
   \<pre>\n"
   ++
   unlines lines1 
   ++
   "\n</pre>\n\
   \         </td>\n\
   \         <td>\n\
   \<pre>\n"
   ++
   unlines lines2
   ++
   "\n</pre>\n\
   \         </td>\n\
   \     </tr>\n\
   \   </table>\n\
   \</center>\n\
   \  </body>\n\
   \</html>\n"
   where
      title = doc1name ++ " vs. " ++ doc2name

-- =================================================================
-- Useful for debugging
-- =================================================================

fred0 = shingles 3 ("fred",[(0,"yabbadabbadoo")])
fred0a = shingles 3 ("fred",[(0,"bbadooyabbada")])
frank0 = shingles 3 ("frank",[(0,"doobeedoobeedoo")])
scooby0 = shingles 3 ("scooby",[(0,"scoobydoobydoo")])

fred1 = kgram 3 winnowNone ("fred",[(0,"yabbadabbadoo")])
fred1a = kgram 3 winnowNone ("fred",[(0,"bbadooyabbada")])
frank1 = kgram 3 winnowNone ("frank",[(0,"doobeedoobeedoo")])
scooby1 = kgram 3 winnowNone ("scooby",[(0,"scoobydoobydoo")])

fred2 = kgram 3 (winnowModP 4) ("fred",[(0,"yabbadabbadoo")])
fred2a = kgram 3 (winnowModP 4) ("fred",[(0,"bbadooyabbada")])
frank2 = kgram 3 (winnowModP 4) ("frank",[(0,"doobeedoobeedoo")])
scooby2 = kgram 3 (winnowModP 4) ("scooby",[(0,"scoobydoobydoo")])

sim1 = sim ["Fib1.java","Fibonacci3.java","Fibonacci4.java"]
sim2 = sim ["fred"]
sim3 = sim_none 3 ["fred","frank"] 
sim4 = sim_none 3 ["fred","frank","scooby"]
sim5 = sim []

viz1 = viz "landskrona1" "landskrona3"
viz2 = viz "fred" "frank"
