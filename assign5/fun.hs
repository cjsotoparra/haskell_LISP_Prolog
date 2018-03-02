import System.Random

intCMP :: Int -> Int -> Ordering
intCMP a b | a == b =EQ
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

type Pair = (Int, Int)

pairCMP :: Pair -> Pair -> Ordering
pairCMP (a,b) (c,d)= if a < c then
                       LT
                     else if a > c then
                       GT
                     else
                       if b < d then
                          LT
                       else if b > d then
                          GT
                       else
                          EQ 

sort3 :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
sort3 _ (x:y:z:a:xs) = error "can't sort more than 3 elements!!!"
sort3 _ [] = []
sort3 _ [a] = [a]
sort3 cmp [a,b] = if cmp a b == LT then
                  [a,b]
                  else
                  [b,a]
sort3 cmp [a,b,c] = if cmp a b == LT then
                       if cmp a c == LT then
                          if cmp b c == LT then
                             [a,b,c]
                          else
                             [a,c,b]
                       else
                          [c,a,b]
                    else if cmp b c == LT then
                            if cmp a c == LT then
                                 [b,a,c]
                            else
                                 [b,c,a]
                    else
                         [c,b,a]


merge :: Ord a => (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge cmp [] [] = []
merge cmp xs [] = xs
merge cmp [] ys = ys
merge cmp xs ys = if cmp (head(xs)) (head(ys)) == LT then
                    head(xs): merge cmp (tail xs) ys
                  else if cmp (head(xs)) (head(ys)) == GT then
                    head(ys): merge cmp xs (tail ys)
                  else
                    head(xs):head(ys):merge cmp (tail xs) (tail ys)

msort2 :: Ord a => (a -> a -> Ordering) -> [a] -> [a] -> [a]
msort2 cmp xs [] = xs
msort2 cmp [] ys = ys
msort2 cmp (x:xs) (y:ys) | cmp x y == LT = x:(msort2 cmp xs (y:ys))
                         | cmp x y == EQ = x:(msort2 cmp xs (y:ys))
                         | otherwise = y:(msort2 cmp (x:xs) ys)

msort :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
msort cmp [] = []
msort cmp [x] = [x]
msort cmp xs = msort2 cmp (msort cmp (fstHalf xs)) (msort cmp (sndHalf xs))

fstHalf :: [a] -> [a]
fstHalf xs = take (length xs `div` 2) xs

sndHalf :: [a] -> [a]
sndHalf xs = drop (length xs `div` 2) xs

makeSet :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
makeSet cmp [] = []
makeSet cmp [a] = [a]
makeSet cmp xs = remDup cmp (msort cmp xs)

remDup :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
remDup cmp [] = []
remDup cmp [a] = [a]
remDup cmp xs | cmp (head xs) (head (tail xs)) == EQ = remDup cmp (tail xs)
              | otherwise =  head xs:remDup cmp (tail xs)

isSet :: Ord a => (a -> a -> Ordering) -> [a] -> Bool
isSet cmp [] = True
isSet cmp [a] = True
isSet cmp [a,b] = if cmp a b == EQ then
                     False
                  else if cmp a b == LT then
                     True
                  else
                     False
isSet cmp xs = if cmp (head xs) (head (tail xs)) == EQ then
                  False
               else if cmp (head xs) (head (tail xs)) == LT then
                  isSet cmp (tail xs)
               else 
                  False

setUnion :: Ord a => (a -> a -> Ordering) -> [a] -> [a] -> [a]
setUnion cmp xs ys = if isSet cmp xs == False then
                        error "arguements must be sets"
                     else if isSet cmp ys == False then
                        error "arguements must be sets"
                     else
                        remDup cmp (msort cmp (xs ++ ys))

setIntersect ::  Ord a => (a -> a -> Ordering) -> [a] -> [a] -> [a]
setIntersect cmp [] ys = []
setIntersect cmp xs [] = []
setIntersect cmp xs ys = if isSet cmp xs == False then
                            error "arguments must be sets"
                         else if isSet cmp ys == False then
                            error "arguements must be sets"
                         else
                            setIntersectH cmp (msort cmp (remDup cmp (xs) ++ remDup cmp (ys)))

setIntersectH ::  Ord a => (a -> a -> Ordering) -> [a] -> [a]
setIntersectH cmp [] = []
setIntersectH cmp [a] = []
setIntersectH cmp xs = if cmp (head xs) (head (tail xs)) == EQ then
                          head(xs):setIntersectH cmp (tail xs)
                       else
                          setIntersectH cmp (tail xs)


remEle :: Ord a => (a -> a -> Ordering) -> [a] -> [a] -> [a]
remEle cmp [] ys = []
remEle cmp xs [] = xs
remEle cmp xs ys = if cmp (head xs) (head ys) == EQ then
                      remEle cmp (tail xs)(tail ys)
                   else
                      head xs: remEle cmp (tail xs) ys

setSubtract ::  Ord a => (a -> a -> Ordering) -> [a] -> [a] -> [a]
setSubtract cmp xs [] = xs
setSubtract cmp [] ys = []
setSubtract cmp xs ys = remEle cmp xs ys

setCrossproduct ::  Ord a => (a -> a -> Ordering) -> [a] -> [a] -> [(a,a)]
setCrossproduct cmp xs ys   | isSet cmp xs == False = error "arguements must be sets" 
                            | isSet cmp ys == False = error "arguements must be sets"
                            | xs == [] = []
                            | ys == [] = []
                            | otherwise = crossProd xs ys

crossProd :: [a] -> [b] -> [(a, b)]
crossProd xs ys = [(x, y) | x <- xs, y <- ys]

setIsSubset :: Ord a => (a -> a -> Ordering) -> [a] -> [a] -> Bool
setIsSubset cmp xs ys | isSet cmp xs == False = error "arguements must be sets"
                      | isSet cmp ys == False = error "arguements must be sets"
                      | otherwise = setIsSubsetHelper cmp xs ys

setIsSubsetHelper :: Ord a => (a -> a -> Ordering) -> [a] -> [a] -> Bool 
setIsSubsetHelper cmp xs [] = False
setIsSubsetHelper cmp [] ys = True
setIsSubsetHelper cmp xs ys = if cmp (head xs) (head ys) == EQ then
                                 setIsSubsetHelper cmp (tail xs) ys
                              else
                                 setIsSubsetHelper cmp  xs (tail ys)

setSimilarity :: Ord a => (a -> a -> Ordering) -> [a] -> [a] -> Double
setSimilarity cmp xs ys = if isSet cmp xs == False then
                            error "arguements must be sets"
                          else if isSet cmp ys == False then
                            error "arguements must be sets"
                          else
                            (fromIntegral (length(setIntersect cmp xs ys))) / (fromIntegral(length (setUnion cmp xs ys)))

setContainment :: Ord a => (a -> a -> Ordering) -> [a] -> [a] -> Double
setContainment cmp xs ys = if isSet cmp xs == False then
                            error "arguements must be sets"
                          else if isSet cmp ys == False then
                            error "arguements must be sets"
                          else
                            (fromIntegral (length (setIntersect cmp xs ys))) / (fromIntegral (length xs))

member :: Ord a => (a -> a -> Ordering) -> a -> [a] -> Bool
member cmp a xs | isSet cmp xs == False = error "arugments must be sets"
                | otherwise  = memberH cmp a xs

memberH :: Ord a => (a -> a -> Ordering) -> a -> [a] -> Bool
memberH cmp a [] = False
memberH cmp a xs = if cmp a (head xs) == EQ then
                      True
                   else 
                      memberH cmp a (tail xs)

siever :: Int -> [Int] -> [Int]
siever n [] = []
siever n xs = if (head xs) `mod` n == 0 then
                  siever n (tail xs)
              else 
                  head(xs):siever n (tail xs)

relprime :: Integral a => a -> a -> Bool
relprime p n = n `mod` p > 0

isPrime :: Int -> Int -> Bool
isPrime n m = if m == 1 then 
               True
              else 
                 if n `mod` m == 0 then
                    False
                 else
                   isPrime n (m-1)

sieve :: [Int] -> [Int]
sieve [] = []
sieve xs = if isPrime (head xs) ((head xs) `div` 2) == True then
              head xs: sieve (tail xs)
           else
              sieve (tail xs)

data Suit = Club | Diamond | Heart | Spade
    deriving (Enum)

data Value =  Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Enum)

type Card = (Suit, Value)

type Deck = [Card]

instance Show Suit where
   show Club = "Club"
   show Diamond = "Diamond"
   show Heart = "Heart"
   show Spade = "Spade"

instance Show Value where
   show Two = "Two"
   show Three = "Three"
   show Four = "Four"
   show Five = "Five"
   show Six = "Six"
   show Seven = "Seven"
   show Eight = "Eight"
   show Nine = "Nine"
   show Ten = "Ten"
   show Jack = "Jack"
   show Queen = "Queen"
   show King = "King"
   show Ace = "Ace"

suitList = [Club, Diamond, Heart, Spade]

valueList = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

makeDeck :: Deck
makeDeck = [(i, j) | i <- [Club, Diamond, Heart, Spade],
                     j <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace] ]

rands :: Int -> Int -> [Float]
rands seed count = take count (randoms (mkStdGen seed) :: [Float])
