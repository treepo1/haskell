--q1
import Data.Char
converte :: Char-> (Char, Char, Int)
converte x 
  |x>= 'a' && x<='z' = (x,minuscMai x, ord (minuscMai x))
  |x>= 'A' && x<='Z' = (x, maiuscMin x, ord (maiuscMin x))
  |otherwise = error "o numero nao esta entre a e z"

dif :: Int 
dif = (ord 'a') - (ord 'A')

maiuscMin :: Char -> Char
maiuscMin a = chr((ord a) + dif)

minuscMai :: Char -> Char
minuscMai a = chr((ord a) - dif)

--q2
type Pessoa = (String, Float, Char)
pessoa :: Float -> Pessoa 
pessoa rg 
  |rg==1 = ("Hugo Benicio Barros",57,'m')
  |rg==2 = ("Josefa Brenda Silva",38,'f')
  |rg==3 = ("Isabelly Rosa",40,'f')
  |rg==4 = ("Otavio Isaac",59,'m')
  |rg==5 = ("Nicolas Castro",51,'m')
  |rg==6 = ("Sophia Stefany Tereza Corte Real",30,'f')
  |rg==7 = ("Maite Rayssa da Costa",18,'f')
  |rg==8 = ("Manuel Fernando Samuel Freitas",78,'m')
  |rg==9 = ("Bernardo Alexandre Galvao",36,'m')
  |otherwise = ("Nao ha mais ninguem",999, 'x')

--(a)
idade :: Pessoa -> Float
idade (x,y,z) = y

nome:: Pessoa-> String
nome (x,y,z) = x

menor :: Pessoa-> Pessoa-> Pessoa
menor x y 
  |idade x <= idade y = x 
  |otherwise = y

menor_idade:: Float-> Pessoa
menor_idade x 
  |x==1 = pessoa 1
  |otherwise = menor (pessoa x) (menor_idade(x-1))

nome_menor_idade:: Float-> String 
nome_menor_idade x = nome(menor_idade (x))

--(b)
soma :: Float -> Float 
soma x
  |x == 0 = 0
  |otherwise = idade(pessoa x) + soma (x-1)

idade_media :: Float -> Float
idade_media x = (soma (x))/x

--(c)
masculino :: Pessoa-> Float
masculino (x,y,z)
  |z == 'm' = 1
  |otherwise = 0

qnts_masc :: Float->Float
qnts_masc x 
  |x == 0 = 0
  |otherwise = masculino(pessoa x) + qnts_masc(x-1)
--(d)
regist :: Pessoa -> Float
regist (x,y,z)
  |(x,y,z) == ("Hugo Benicio Barros",57,'m') = 1
  |(x,y,z) == ("Josefa Brenda Silva",38,'f') = 2
  |(x,y,z) == ("Isabelly Rosa",40,'f') = 3
  |(x,y,z) == ("Otavio Isaac",59,'m') = 4
  |(x,y,z) == ("Nicolas Castro",51,'m') = 5
  |(x,y,z) == ("Sophia Stefany Tereza Corte Real",30,'f') = 6
  |(x,y,z) == ("Maite Rayssa da Costa",18,'f') = 7
  |(x,y,z) == ("Manuel Fernando Samuel Freitas",78,'m') = 8
  |(x,y,z) == ("Bernardo Alexandre Galvao",36,'m') = 9
  |otherwise = 0
 
maior :: Pessoa -> Pessoa -> Pessoa
maior x y 
  |idade x >= idade y = x 
  |otherwise = y

maior_idade :: Float-> Pessoa
maior_idade x 
  |x==1 = pessoa 1 
  |otherwise = maior (pessoa x) (maior_idade(x-1))

regist_maior_idade:: Float-> Float
regist_maior_idade x = regist(maior_idade(x))


--q3

analisaLetra :: Char-> (Char, Char, Int)
analisaLetra x 
  |x>= 'a' && x<='z' = (x,minuscMai x, ord x)
  |x>= 'A' && x<='Z' = (x, maiuscMin x, ord x)
  |otherwise = error "o numero nao esta entre a e z"                                          
              
--q4 
type Ordenados = (Int,Int,Int,Int,Int)
ordena:: Int->Int->Int->Int ->Int-> Ordenados
ordena a b c d e
  |a > b = ordena b a c d e
  |b > c = ordena a c b d e
  |c > d = ordena a b d c e
  |d > e = ordena a b c e d
  |otherwise = (a,b,c,d,e)



