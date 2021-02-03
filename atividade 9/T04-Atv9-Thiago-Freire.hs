--q1
import Data.Char

transformar :: (a -> t) -> [a] -> [t]
transformar f [] = []
transformar f (x:xs) = (f x) : (transformar f xs)

dif :: Int 
dif = (ord 'a') - (ord 'A')

all_to_min a 
  |elem a ['a'..'z'] = a
  |otherwise = chr((ord a) + dif)

all_to_mai a 
  |elem a ['A'..'Z'] = a
  |otherwise = chr((ord a) - dif)

dic_10 = [(0,"zero"),(1,"um"),(2,"dois"),(3,"tres"),(4,"quatro"),(5,"cinco"),(6,"seis"),(7,"sete"),(8,"oito"), (9,"nove")]

primeiro (a,b) = a 
segundo (a,b) = b

traduzaux [] _ = []
traduzaux (a:x) n 
  |n == (primeiro a) = segundo a
  |otherwise = traduzaux x n 

num_to_string n = traduzaux dic_10 n 

--q2 
total f n 
  |n == 0 = f 0 
  |otherwise = f n + total f (n-1)

--q3
mapfiltrar p f x = map f (filter p x)

eh_par n = (mod n 2 == 0)

vezes_3 n = n*3

--q4
primo n 
 |n == 1 = False
 |n < 1 = error "nao eh um numero positivo"
 |otherwise = check_primo n == n

check_primo a = check_div a 2

check_div x y
 |divisivel x y = y
 |y^2 > x = x
 |otherwise = check_div x (y+1)

divisivel c d = mod c d == 0



