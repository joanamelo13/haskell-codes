module Aula1 where

-- exercitando funcoes apresentadas na aula 1 de Programacao Funcional
-- compiladas e testadas no ghci

import Prelude ((+), (*), (==), Num, Int, Eq, Bool(True, False), error, Functor (fmap) , ( <$>) , Applicative ( pure , (<*>)))


--retorna o comprimento de uma lista
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

--retorna um boolean para informar se a lista é vazia ou nao
null :: [a] -> Bool
null [] = True
null (x:xs) = False

-- retona a cabeça da lista
head :: [a] -> a
head [] = error "head: empty list"
head (x:_) = x

--retorna a cauda da lista
tail :: [a] -> [a]
tail [] = error "tail: empty list"
tail (_:xs) = xs

--retorna apenas o ultimo elemento
last :: [a] -> a
last [] = error "last: empty list"
last (x:[]) = x
last (_:xs) = last xs

--retorna todos os elementos exceto o ultimo
init :: [a] -> [a]
init [] = error "init: empty list"
init (_:[]) = []
init (x:xs) = x : init xs

--operacao de concatenacao que retona uma lista concatenada de outras duas listas
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:xs ++ ys

--funcao que retorna a concatena de duas listas de um mesmo tipo recebendo uma lista de listas
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

--retorna uma lista em ordem reversa (nao pode ter duas funcoes com o mesmo nome por isso esta comentada ela utiliza concatenacao)
--reverse :: [a] -> [a]
--reverse [] = []
--reverse (x:xs) = reverse xs ++ [x]

-- retorna uma lista em ordem reversa de forma recursiva (ate agora eh a mais chatinha porem eh a forma mais correta)
reverse :: [a] -> [a]
reverse xs = f xs [] where
	f [] acc = acc
	f (x:xs) acc = f xs (x:acc)



