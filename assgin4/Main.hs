g n
   | (n `mod` 3) == x = x
   | (n `mod` 3) == y = y
   | (n `mod` 3) == z = z
      where
        x = 0
        y = 1
        z = 2

f [] = []
f xs =
   let
     square a = a * a
     one = 1
     (y:ys) = xs
   in 
     (square y + one) : f ys

doublestring :: String -> String
doublestring [] = []

type Rat = (Int , Int)

signum n | n == 0 = 0
         | n < 0 = -1
         | n > 0 = 1

normRat :: Rat -> Rat
normRat (_,0) = error "Bad!"
normRat (0,_) = (0, 1)
normRat (x,y) = (a `div` d, b `div` d)
                 where a = (Main.signum y) * x
                       b = abs y
                       d = gcd a b

addRat (a,b) (c,d) = normRat(a*b + c*d, b*d)
subRat (a,b) (c,d) = normRat(a*b - c*d, b*d)
mulRat (a,b) (c,d) = normRat (a*c, b*d)
divRat (a,b) (c,d) = normRat (a*d, b*c)

eqRat (a,b) (c,d) =
       let x = normRat (a,b) in
       let y = normRat (c,d) in
          (fst x == fst y) && (snd x == snd y)
