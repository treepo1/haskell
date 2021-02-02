--q1
fatorial n = fataux n 1
fataux 0 x = x
fataux n x = fataux (n-1) (x*n)

comb m n 
  |m < n = error "o valor de m nao pode ser menor que n"
  |otherwise = (fatorial (m))/(fatorial(n)*(fatorial(m-n)))

--q2
mdc :: Int -> Int -> Int 
mdc m n 
  | m == 0 = n
  | m > 0 = mdc (n `mod` m) m
  |otherwise = 0 
 
--q3
mmc1 :: Int-> Int-> Int
mmc1 a b = a*b `div` mdc a b 
mmc :: Int-> Int -> Int -> Int
mmc x y c = mmc1 (mmc1 (x) (y)) (c)

--q4
raizint x = truncate(sqrt(x))

--q5
a :: Int-> Int-> Int
a m n 
  |m == 0 = n +1 
  |m /= 0 && n == 0 = a(m-1) (1)
  |m /= 0 && n /= 0 = a(m-1) (a(m)(n-1))
  |otherwise = error "erro"