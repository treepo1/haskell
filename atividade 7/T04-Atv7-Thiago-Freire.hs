--Q1
maior [a] = a
maior (a:x)
  |a > (maior x) = a
  |otherwise = (maior x)

posicao2 (a:x) b 
  |b==a = 0 
  |otherwise = 1 + posicao2 x b 

maior_posicao :: [Int]-> (Int,Int)
maior_posicao x = (maior x, posicao2 x (maior x))

--Q2

dic_10 = [(0,"zero"),(1,"um"),(2,"dois"),(3,"tres"),(4,"quatro"),(5,"cinco"),(6,"seis"),(7,"sete"),(8,"oito"), (9,"nove")]

primeiro (a,b) = a 
segundo (a,b) = b

traduzaux [] _ = []
traduzaux (a:x) n 
  |n == (primeiro a) = segundo a
  |otherwise = traduzaux x n 
 
traduz n = traduzaux dic_10 n 

--Q3
maior_idade :: [(String,Int)]-> Int
maior_idade x = maior (segundo (unzip x))

menor_idade :: [(String,Int)]-> Int
menor_idade x = menor (segundo (unzip x))

maisVelho :: [(String,Int)] -> String
maisVelho lista =
  case lista of
      [a] -> primeiro a
      []  -> "nao ha ninguem"
      (a,b):xs -> if (maior_idade lista) == b then a else maisVelho xs

maisNovo :: [(String,Int)] -> String
maisNovo lista = 
  case lista of
      [a] -> primeiro a
      []-> "nao ha ninguem"
      (a,b):xs -> if (menor_idade lista) == b then a else maisNovo xs

--Q4
deleta :: Int-> [Int] -> [Int]
deleta c (a:x)
  |c==a = x
  |otherwise = (a: deleta c x)

del_posicao :: [Int]-> Int-> [Int]
del_posicao x y = deleta (posicao x y) x

--Q5
filtrar n [] = []
filtrar n (a:x)
  |n a = a:filtrar n x
  |otherwise = filtrar n x

menor [a] = a
menor (a:x)
  |a < (menor x) = a
  |otherwise = (menor x)

impar n = (n `mod` 2 /= 0)
ordena [] = []
ordena (a:x) = filtrar impar [menor (a:x)] ++ ordena (deleta (menor(a:x)) (a:x))

--Q6  
pegar n _ 
  |n <= 0 = []
pegar _ [] = []
pegar n (a:x) = a : pegar (n-1) x

tirar 0 (a:x) = (a:x)
tirar _ [] = []
tirar n (a:x) = tirar (n-1) x

insere_posicao x y z = pegar z x ++ y : tirar z x
  
--Q7
cabeca (a:x) = a 

tamanho [] = 0 
tamanho (a:x) = 1 + tamanho x

posicao :: [a] -> Int -> a
posicao x y
  | y > tamanho x - 1 = error "Posição maior que o tamanho da lista"
  | otherwise = cabeca(tirar y x)
      
--Q8
repeteaux a n 
  |n == 0 = [] 
  |n == 1 = [a]
  |otherwise =  a : (repeteaux a(n-1)) 

repete 1 = [1]
repete c = repeteaux c c ++ repete (c-1)

--Q9
inverte :: [a]-> [a]
inverte [] = []
inverte (a:x) = (inverte x) ++ [a]

palindromo (x:y)
  |(x:y) == (inverte (x:y)) = True
  |otherwise = False