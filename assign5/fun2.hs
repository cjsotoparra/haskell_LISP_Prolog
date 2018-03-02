intCMP :: Int -> Int -> Ordering
intCMP a b | a == b =EQ
           | a < b = LT
           | otherwise = GT

intCMPRev :: Int -> Int -> Ordering
intCMPRev a b | a == b = EQ
              | a < b = GT
              | otherwise = LT

sort3 :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
sort3 _ (x:y:z:a:xs) = error "can't sort more than 3 elements!!!"
sort3 _ [] = []
sort3 _ [a] = [a]
sort3 cmp [a,b] = if cmp a b == LT then
                  [a,b]
                  else
                  [b,a]
sort3 cmp [a,b,c] = if cmp a b == GT && cmp c b == LT then
                       [c,b,a]
                    else if cmp a b == GT && cmp a c == LT then
                       [b,a,c]
                    else if cmp a b == EQ && cmp b c == EQ then
                       [a,a,a]
                    else
                       [a,b,c]
  

