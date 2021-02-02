
--q1
type Data = (Int,Int,Int)

bissexto :: Int -> Bool
bissexto n 
  |n `mod` 400 == 0 = True
  |n `mod` 4 == 0 && n `mod` 100 /=0 = True
  |otherwise = False

dia :: Int-> Int
dia n
  |n <= 31 = n
  |otherwise = error "dia invalido"

ano :: Int-> Int-> Int
ano ano_menor ano_maior
  |ano_menor == ano_maior = 0
  |ano_menor < ano_maior && bissexto ano_menor == True = 366 + ano (ano_menor +1) ano_maior
  |ano_menor > ano_maior = error "ordem invalida, menor data primeiro"
  |otherwise = 365 + ano (ano_menor +1) ano_maior

mes_bissexto :: Int -> Int
mes_bissexto mes 
  |mes == 1 = 0 
  |mes == 2 = 31 
  |mes == 3 = 60
  |mes == 4 = 91
  |mes == 5 = 121
  |mes == 6 = 152
  |mes == 7 = 182
  |mes == 8 = 213
  |mes == 9 = 244
  |mes == 10 = 274
  |mes == 11 = 305
  |mes == 12 = 335

mes1 :: Int -> Int
mes1 mes 
  |mes == 1 = 0 
  |mes == 2 = 31 
  |mes == 3 = 59
  |mes == 4 = 90
  |mes == 5 = 120
  |mes == 6 = 151
  |mes == 7 = 181
  |mes == 8 = 212
  |mes == 9 = 243
  |mes == 10 = 273
  |mes == 11 = 304
  |mes == 12 = 334

data1 :: Data -> Int
data1 (a,b,c) 
  |c `mod` 400 == 0 = dia a + mes_bissexto b
  |c `mod` 4 == 0 && c `mod` 100 /= 0 = dia a + mes_bissexto b
  |otherwise = dia a + mes1 b 

dif_dias :: Data -> Data -> Int
dif_dias (a,b,c) (x,y,z) = data1(x,y,z) - data1 (a,b,c) + ano c z 


--q2
delta:: (Float,Float, Float) -> Float
delta (a,b,c) = (b)**2 + ((-4)*a*c)  

bhaskara :: (Float, Float, Float) -> (Float, Float)   
bhaskara (x,y,z) 
  |delta (x,y,z) >= 0 = ((negate(y) + sqrt(delta(x,y,z)))/(2*x), (negate(y) - sqrt(delta(x,y,z)))/(2*x))                                                         
  |otherwise = error "raizes imaginarias"

--q3 
perimetro :: (Float, Float,Float)-> Float
perimetro (a,b,c) = a+b+c

tipo_triang :: (Float,Float,Float) -> String
tipo_triang (a,b,c)
  |a == b || a == c || c == b = "Isoceles"
  |a /= b && a /= c && b /= c = "Escaleno"
  |a == b && a == c && b == c = "Equilatero"
  |otherwise = "invalido"

verifica :: (Float, Float, Float)-> (String, Float)
verifica (a,b,c)
  |abs(b-c) < a && a < b + c = (tipo_triang(a,b,c), perimetro(a,b,c))
  |abs(a-c) < b && b < a + c = (tipo_triang(a,b,c), perimetro(a,b,c))
  |abs(a-b) < c && c < a + b = (tipo_triang(a,b,c), perimetro(a,b,c))
  |otherwise = ("Nao forma triangulo", 000)
  
--q4 
type Dados = (Int, String, String, Char)
base :: Int -> Dados 
base x 
  |x== 1 = (1793, "Pedro Paulo", "MESTRE", 'M') 
  |x== 2 = (1797, "Joana Silva Alencar", "MESTRE", 'F') 
  |x== 3 = (1534, "Joao De Medeiros", "DOUTOR", 'M') 
  |x== 4 = (1298, "Claudio Cedar de Sa", "DOUTOR", 'M') 
  |x== 5 = (1737, "Paula de Medeiros", "MESTRE", 'F') 
  |x== 6 = (1888, "Rita de Matos", "MESTRE", 'F') 
  |x== 7 = (1698, "Tereza Cristina Andrade", "MESTRE", 'F')
  |x== 8 = (1267, "Paulo Roberto e Silva Carvalho", "MESTRE", 'M')
  |x== 9 = (1598, "Leticia Nunes Pereira", "DOUTOR", 'F')
  |x== 10 =(1798, "Epaminondas Ferreira da Silva", "DOUTOR", 'M')


--(a)
detecta_dout :: Dados-> Int 
detecta_dout (x,y,z,c) 
  |z == "DOUTOR" = 1
  |otherwise = 0 

doutores1 :: Int-> Int 
doutores1 x 
  |x == 0 = 0
  |otherwise = detecta_dout(base x) + doutores1(x-1)

doutores :: Int
doutores = doutores1 numbase

--(b)
detecta_f :: Dados-> Int 
detecta_f (x,y,z,c) 
  |c == 'F' = 1
  |otherwise = 0 

mulheres1 :: Int-> Int
mulheres1 x
  |x == 0 = 0 
  |otherwise = detecta_f(base x) + mulheres1(x-1)

numbase :: Int 
numbase = 10
mulheres :: Int
mulheres = mulheres1 numbase

--(c)
decta_m_mestre :: Dados-> Int 
decta_m_mestre (x,y,z,c) 
  |z == "MESTRE" && c == 'M' = 1
  |otherwise = 0 

mestre_masc2 :: Int -> Int
mestre_masc2 x 
  |x == 0 = 0 
  |otherwise = decta_m_mestre(base x) + mestre_masc2(x-1)

mestre_masc :: Int 
mestre_masc = mestre_masc2 numbase

--(d) 

nome :: Dados -> String
nome (a,b,c,d) = b 

matricula :: Dados -> Int
matricula (a,b,c,d) = a

menor :: Dados ->Dados-> Dados
menor x y
  |matricula(x)< matricula(y) = x 
  |otherwise = y 

menor_matricula :: Int-> Dados
menor_matricula x  
  |x==1 = base 1
  |otherwise = menor (base x) (menor_matricula(x-1))

nome_menor_matricula :: Int-> String
nome_menor_matricula x = nome(menor_matricula(x))

nome_mais_antigo :: String
nome_mais_antigo = nome_menor_matricula numbase


