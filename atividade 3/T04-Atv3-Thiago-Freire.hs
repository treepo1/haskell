--q1
--a
fatorial n
  |n == 0 = 1
  |otherwise =  n* fatorial(n-1)

taylor n x 
  |n == 0 = 1
  |otherwise = (x**n)/(fatorial n)  + (taylor(n-1) x)

--b
erro n x 
  |exp x - taylor n x <= 0.001 = 0
  |otherwise = 1 + erro (n+1) x

--q2
resto1 :: Int-> Int-> Int
resto1 a b 
  |b == 0 = error "Indeterminação"
  |a < b = a
  |otherwise = resto1 (a-b) (b)

--q3
--a
an :: Int-> Double
an n 
  |n == 1 = sqrt 6
  |otherwise = sqrt(6+ an(n-1))
--b
soma :: Int-> Double
soma n 
  |n == 1 = an 1
  |otherwise = an 1 + soma(n-1)

