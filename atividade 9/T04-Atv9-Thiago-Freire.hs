--q1
import Data.Char

dif :: Int 
dif = (ord 'a') - (ord 'A')

maiuscMin a = chr((ord a) + dif)
minuscMai a = chr((ord a) - dif)

transform_all_to_min [] = []
transform_all_to_min (a:x)
  |elem a ['A'..'Z'] == True = maiuscMin a : transform_all_to_min x
  |otherwise = a: transform_all_to_min x

transform_all_to_maiusc [] = []
transform_all_to_maiusc (a:x)
  |elem a ['a'..'z'] == True = minuscMai a : transform_all_to_maiusc x
  |otherwise = a: transform_all_to_maiusc x


dic_10 = [(0,"zero"),(1,"um"),(2,"dois"),(3,"tres"),(4,"quatro"),(5,"cinco"),(6,"seis"),(7,"sete"),(8,"oito"), (9,"nove")]

primeiro (a,b) = a 
segundo (a,b) = b

traduzaux [] _ = []
traduzaux (a:x) n 
  |n == (primeiro a) = segundo a
  |otherwise = traduzaux x n 
 
traduz n = traduzaux dic_10 n 

transformar_num_to_string n = map traduz n 

--q2 
total f n 
  |n == 0 = f 0 
  |otherwise = f n + total f (n-1)

--q3
mapfiltrar p f x = map f (filter p x)
eh_par n = (mod n 2 == 0)
vezes_3 n = n*3


--q4
eh_primo n 
 | n < 1 = error "nao eh um numero positivo"
 | n == 1 = False
 | otherwise = verifPrimo n == n

verifPrimo n = verifPrimoIni n 2

verifPrimoIni n c 
 |resto n c = c
 |c^2 > n = n
 |otherwise = verifPrimoIni n (c+1)

resto n d = mod n d == 0
