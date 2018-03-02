import Data.Char
doublestring :: String -> String
doublestring n | n == [] = []
               | otherwise = n ++ n

charToString :: Char -> String
charToString c = [c]

cylinderSurfaceArea :: (Float, Float) -> Int
cylinderSurfaceArea (r,h) = floor((2 * pi * r * h) + (2 * pi * (r * r)))

third :: [Int] -> Int
third xs = head (tail (tail xs) )

yahtzee :: [Int] -> Bool
yahtzee xs =  xs == [head xs, head xs, head xs, head xs, head xs]

msum :: Int -> Int
msum n = if n == 0 then
            0
         else 
            n + msum (n-1)

gsum:: Int -> Int
gsum n | n == 0 = 0
       | otherwise = n + gsum(n-1)

copystring :: (String,Int) -> String
copystring (s,n) | n <= 0 = []
                 | otherwise = s ++ copystring (s, n-1)

numlist :: Int -> [Int]
numlist n | n < 0 = error "program error: illegal argument"
          | n == 0 = [n]
          | otherwise = reverse(n:reverse(numlist(n-1)))

allsame :: [Int] -> Bool
allsame xs | length xs <= 1 = True
           | head xs /= head (tail xs) = False
           | otherwise =  allsame(tail xs)

swap :: [Int] -> [Int]
swap xs | length xs <= 1 = xs
        | otherwise =  head(tail xs):head xs:swap(tail(tail xs))

split :: [Int] -> ([Int], [Int])
split [] = ([],[])
split [x] = ([x], [])
split ([x,y]) = ([x],[y])
split (x:y:xys) = (x:xs, y:ys) where (xs, ys) = split xys

string2IntList :: String -> [Int]
string2IntList xs | xs == "" = []
                  | otherwise = ord (head xs) - 48:string2IntList(tail xs)

sumDigits :: Int -> Int
sumDigits x | x < 10 = x
            | otherwise = (x `div` 10) + (x `mod` 10)

sumDigitsList :: [Int] -> Int
sumDigitsList xs | length xs == 0 = 0
                 | length xs == 1 = sumDigits(head xs)
                 | otherwise = sumDigits(head xs) + sumDigitsList(tail xs)

double :: [Int] -> [Int]
double xs | length xs ==0 = []
          | otherwise = (head xs)+(head xs): double (tail xs)

everyOtherH :: ([Int],Int) -> [Int]
everyOtherH (xs, n) | xs == [] = xs
                    | n `mod` 2 == 0 = head(xs):everyOtherH((tail xs), n+1)
                    | otherwise = everyOtherH((tail xs), n+1)    

everyOther :: [Int] -> [Int]
everyOther xs | xs == [] = []
              | length xs == 1 = xs
              | otherwise = everyOtherH(xs, 0)

luhnSumH :: ([Int], Int) -> [Int]
luhnSumH (xs, n) | xs == [] = xs
                 | n `mod` 2 == 0 = head(xs):luhnSumH((tail xs), n+1)
                 | otherwise = sumDigits(2*(head xs)): luhnSumH((tail xs), n+1)                 


luhnSum :: [Int] -> Int
luhnSum xs = sumDigitsList(luhnSumH(reverse(xs), 0))

isValidCC :: String -> Bool
isValidCC x = luhnSum (string2IntList x) `mod` 10 == 0

nameEQ :: ((a, a),(b, b)) -> Bool
nameEQ (a, _) (b, _) | a == b = True

member :: Eq a => (a -> a -> Bool) -> a -> [a] -> Bool
member eq x ys | eq == nameEQ = nameEQ (x, head(ys))
