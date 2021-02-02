--q1
menor [a] = a
menor (a:x) 
  |a < menor x = a 
  |otherwise = menor x 

deleta :: Int-> [Int] -> [Int]
deleta c (a:x)
  |c==a = x
  |otherwise = (a: deleta c x)

ordena [] = []
ordena (a:x) = [menor (a:x)] ++ ordena (deleta (menor(a:x)) (a:x))

--q2
fib1 1 = 1
fib1 0 = 0
fib1 n = fib1 (n-1) + fib1 (n-2)

fib2 :: Int -> [Int]
fib2 n 
  |n == 0 = [0]
  |otherwise = fib1 n : (fib2 (n-1))

fib n = reverse(fib2 n)

--q3
merge :: [Int]-> [Int]-> [Int]
merge (a:x) (b:c) = ordena((a:x) ++ (b:c))

--q4
busca_sub _ [] = []
busca_sub [] _ = []
busca_sub (x:xs) (y:ys)
  |take (length (x:xs)) y == (x:xs) = y:busca_sub (x:xs) ys
  |otherwise = busca_sub (x:xs) ys

--q5
multiplos :: [Int]-> [Int] -> [Int] -> [Int]
multiplos [] n _= n 
multiplos (x:xs) [] n = multiplos xs n []
multiplos (x:xs) (y:ys) n
 |mod y x == 0 = multiplos (x:xs) ys (y:n)
 |otherwise = multiplos (x:xs) ys n

filtro:: [Int] ->[Int] ->[Int]
filtro a b = (multiplos a b [])

--q6
intersec [] _ = []
intersec (x:xs) (y:ys) 
  |elem x (y:ys) == True = x:intersec xs (y:ys)
  |otherwise = intersec xs (y:ys)

--q7
rodar_direita x y
    | x > length y = rodar_direita (x - length y) (reverse y)
    | x /= 0 = last y :rodar_direita (x-1) (init y)
    | otherwise = y

--q8
comprime [] = []
comprime [x] = [x]
comprime (x:xs)
  |len > 3 = ("!" ++ show len ++ [x]) ++ comprime tirarepet
  |otherwise = pegarepet ++ comprime tirarepet
 where 
  len = length(pegarepet)
  tirarepet = (tiraenquanto (==x) (x:xs))
  pegarepet = (pegaenquanto (==x) (x:xs))

pegaenquanto:: (a -> Bool) -> [a] -> [a]
pegaenquanto _ [] =  []
pegaenquanto p (x:xs)
            | p x =  x : pegaenquanto p xs
            | otherwise = pegaenquanto p xs

tiraenquanto:: (a -> Bool) -> [a] -> [a]
tiraenquanto _ [] =  []
tiraenquanto p (x:xs)
            | p x =  dropWhile p xs
            | otherwise =  xs

--q9 
repeteaux a n 
  |n == 0 = [] 
  |n == 1 = [a]
  |otherwise =  a : (repeteaux a(n-1)) 

descomprime [] = []
descomprime ('!':n:c:x) = repeteaux (c) (read [n]) ++ descomprime x
descomprime (a:x) = a:descomprime x











