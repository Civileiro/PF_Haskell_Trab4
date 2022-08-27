-- Nome: Gabriel Prost Gomes Pereira

import Data.Char

formatExample :: [Char] -> [Char] -> [Char] -> [Char]
formatExample n arg result = "Func." ++ n ++ ":entrada:" ++ arg ++ ":resultado:" ++ result

showExample :: (Show a, Show b, Show c) => a -> [b] -> c -> [Char]
showExample n arg result = formatExample nS argS resultS
  where
  nS = [x | x <- show n, notElem x "\"\\"]
  argS = [x | x <- unwords (map show arg), notElem x "\"\\"]
  resultS = show result

main = do
  print (showExample 1 [10] (fatorialn 10))
  print (showExample 2 [[-0.4, 5.5, 0, 2.2]] (quadradoReal [-0.4, 5.5, 0, 2.2]))
  print (showExample 3 ["['oi,', 'tudo', 'bem por ai?']"] (comprimentoPalavras ["oi,", "tudo", "bem por ai?"]))
  print (showExample 4 [""] maiorMultiploDe29)
  print (showExample 5 [1] (maiorMultiploDe 1))
  print (showExample 5 [2] (maiorMultiploDe 2))
  print (showExample 5 [3] (maiorMultiploDe 3))
  print (showExample 5 [111] (maiorMultiploDe 111))
  print (showExample 5 [-6] (maiorMultiploDe (-6)))
  print (showExample 6 [2] (somaQuadrados 2))
  print (showExample 6 [3] (somaQuadrados 3))
  print (showExample 6 [6] (somaQuadrados 6))
  print (showExample 6 [12] (somaQuadrados 12))
  print (showExample 7 [[1, 2, 3]] (comprimento [1, 2, 3]))
  print (showExample 7 ["[]"] (comprimento []))
  print (showExample 7 ["[1..1000]"] (comprimento [1..1000]))
  print (showExample "8-flip(<)" [2, 6] (flip (<) 2 6))
  print (showExample "8-flip(^)" [3, 5] (flip (^) 3 5))
  print (showExample "8-ord1" ['!'] (ord '!'))
  print (showExample "8-ord2" ['z', 'a'] (testOrd2 'z' 'a'))
  print (showExample "8-ord2" ['a', 'A'] (testOrd2 'a' 'A'))
  print (showExample "8-max" [1, 10] (max 1 10))
  print (showExample "8-max" [10, -1] (max 10 (-1)))
  print (showExample "8-min" [1, 10] (min 1 10))
  print (showExample "8-min" [10, -1] (min 10 (-1)))
  print (showExample "8-curry fst" [3, -6] (curry fst 3 (-6)))
  print (showExample "8-curry snd" [3, -6] (curry snd 3 (-6)))
  print (showExample "8-uncurry(+)" [(3, -6)] (uncurry (+) (3, -6)))
  print (showExample "8-uncurry(*)" [(3, -6)] (uncurry (*) (3, -6)))
  

{-
1. Escreva  uma  função  chamada  fatorialn  que  usando  o  operador  range  e  a  função  foldr devolva o fatorial de n.
-}
fatorialn :: Integer -> Integer
fatorialn n = foldr (*) 1 [1..n]
-- replit fala pra usar product @_@

{-
2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos reais listados.
-}
quadradoReal :: Real r => [r] -> [r]
quadradoReal = map (^2)

{-
3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras.
-}
comprimentoPalavras :: [[Char]] -> [Int]
comprimentoPalavras [] = []
comprimentoPalavras (x:xs) = length x : comprimentoPalavras xs

{-
4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29.
-}
divisibleBy :: Integer -> Integer -> Bool
divisibleBy x y = rem y x == 0

maiorMultiploDe29 :: Integer
maiorMultiploDe29 = last (filter (divisibleBy 29) [0..100000])

{-
5. Usando  a  função  filter  escreva  uma  função,  chamada  maiorMultiploDe que  recebe  um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro. 
-}
maiorMultiploDe :: Integer -> Integer
maiorMultiploDe x = last (filter (divisibleBy x) [0..100000])

{-
6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=1² +2² +3² +4²...+𝑛². 
-}
somaQuadrados :: Integer -> Integer
somaQuadrados n = foldr (+) 0 [x^2| x <- [1..n]]

{-
7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada. 
-}
increase :: Integer -> a -> Integer
increase z _ = z + 1 

comprimento :: [a] -> Integer
comprimento = foldl increase 0

{-
8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos. 
-}
testOrd2 :: Char -> Char -> Int
testOrd2 a b = ord a - ord b