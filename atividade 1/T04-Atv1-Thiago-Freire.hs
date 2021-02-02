--Q1
numeros a b c
  |(a == b) && (b == c) = 3
  |(a == b) = 2
  |(a == c) = 2
  |(c == b) = 2
  |otherwise = 0
--Q2
media a b c = (a + b + c) / 3 
comparamedia a b c | (a > (media a b c)) && (b > (media a b c)) && (c > (media a b c)) = 3
        | (a > (media a b c)) && (b > (media a b c)) = 2
        | (a > (media a b c)) && (c > (media a b c)) = 2
        | (b > (media a b c)) && (c > (media a b c)) = 2
        | (a > (media a b c)) = 1
        | (b >(media a b c)) = 1
        | (c > (media a b c)) = 1
        | otherwise = 0
--Q3

potencia_2 x = x * x

--Q4

potencia_4 x = potencia_2(potencia_2(x))

--Q5

exclusiva :: Bool -> Bool -> Bool
exclusiva a b 
      |(a == True) && (b == True) = False
      |(a == True) && (b == False) = True
      |(a == False) && (b == True) = True
      |(a == False) && (b == False) = False

--Q6

bhaskara :: Float -> Float -> Float -> (Float , Float)
bhaskara a b c
 |d < 0 = error "Raizes negativas!"
 |otherwise = (x1 , x2)
 where
  x1 = (-b - (sqrt d))/(2*a) 
  x2 = (-b + (sqrt d))/(2*a)
  d  = (b**2) -4*a*c
  

        
