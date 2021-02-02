--q1
soma1 :: Int -> Int -> Int
soma1 a b 
  |b == a = a
  |a == b = b
  |a < b = soma1 (a) (b-1) + b
  |otherwise = soma1 (b) (a-1) + a

soma2 :: Int-> Int -> Int
soma2 a b 
  |a<b = soma1 (a) (b-1)+b -a-b
  |otherwise = soma1 (b) (a-1)+a -a-b 

--q2

multsEntre :: Int -> Int -> Int -> Int
multsEntre n1 n2 n3
  | n1 > n2 = 0
  | otherwise =  if mod n1 n3 == 0
                 then 1 + nroMults
                 else nroMults
     where
       nroMults = multsEntre (n1+1) n2 n3

--q3
mult :: Int -> Int -> Int
mult x y
  |y == 0 = 0
  |y > 0  = x + mult x (y-1)
  |otherwise = negate (mult x (negate y))

