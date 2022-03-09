module Entrega1 where

import Prelude (
	(+), (*), (==), (/), (>), (<=), (>=),
	Num, Int, Eq, Ord, Bool(True, False), 
	error, Fractional, Either)

import List
import Bool
import Functions
import Maybe

{-Joana Maria Chaves Melo - 415604 
Este arquivo contem as questoes referentes a entrega 1 de programação funcional dos labs 1,2 e 3, tambem contem as questoes das notas de aula.
Todas as funções presentes nesse código foram testadas para casos de listas vazias e listas com cauda e cabeca, ou so cabeca.-}



{-*************************Lab 1*************************-}

{- 1. Reescreva as funcoes length, (++), concat e reverse como chamadas de foldl ou foldr (a que parecer mais adequada).-}

length :: [a] -> Int
length = foldl f 0 where
    f acc _ = acc + 1

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat (xs:xss) = foldl (++) xs xss

reverse :: [a] -> [a]
reverse = foldl f [] where
    f xs x = x:xs

{- 2. Escreva a funcao squares que, dada uma lista de inteiros, retorna uma lista contendo o quadrado desses inteiros.-}

squares :: [Int] -> [Int]
squares [] = []
squares (x:xs) = x * x:squares xs


{-3. Escreva a funcao count :: Eq a => a -> [a] -> Int, que retorna o numero de ocorrencias de um elemento em uma lista. 
obs.: verificar como está a função cond no Bool.hs-}

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x (y:ys) = cond (x == y) 1 0 + count x ys


{-*************************Lab 2*************************-}

{- 1. Escreva a funcao repeat :: a -> [a] que, dado um elemento x de tipo a, retorna uma lista infinita cujos elementos sao todos iguais a x.-}

repeat :: a -> [a]
repeat x = x : repeat x


{- 2. Escreva a funcao cycle :: [a] -> [a] que, dada uma lista xs como entrada, 
retorna uma lista infinita formada por repetir continuamente os elementos de xs, em ordem. Desejamos que cycle [] == [].-}

cycle :: [a] -> [a]
cycle [] = []
cycle xs = xs ++ cycle xs


{- 3. Escreva a funcao intercalate :: a -> [a] -> [a] que, dado um elemento x e uma lista ys, retorna uma lista que consiste dos elementos de ys intercalados por x. 
Desejamos que intercalate [] == [] e intercalate [y] == [y], pois nesses casos ys nao tem elementos a serem intercalados por x.-}

intercalate :: a -> [a] -> [a]
intercalate _ [] = []
intercalate _ [y] = [y]
intercalate x (y:ys) = y : x : intercalate x ys

{- 4. Escreva a funcao safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a que faz divisoes de forma segura, isto e, representa a divisao por zero como uma falha.-}

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = error "safeDiv: isn't possible to divide by zero"
safeDiv x y = Just (x / y)


{- 5. Escreva uma implementacao para find :: (a -> Bool) -> [a] -> Maybe a. Perceba que e possıvel implementar find utilizando composicoes de dropWhile, 
safeHead e not. Essa e apenas uma sugestao. Implemente como achar mais adequado.-}

find :: (a -> Bool) -> [a] -> Maybe a
find y = safeHead . dropWhile (not . y)



{-*************************Lab 3*************************-}

{-1. Implemente a funcao group :: Eq a => [a] -> [[a]], que recebe uma lista xs e retorna uma lista de listas, onde cada sublista e uma subsequencia contınua maximal
de elementos iguais de xs. Por exemplo, group [1, 1, 2, 2, 2, 3, 3, 1, 1, 1] == [[1, 1], [2, 2, 2], [3, 3], [1, 1, 1]]. A funcao span pode ser de grande ajuda aqui.-}

group :: Eq a => [a] -> [[a]]
group = groupBy (==)


{-2. Agora, generalizamos a funcao group como groupBy, fazendo com que group seja equivalente a groupBy (==). Implemente groupBy :: (a -> a -> Bool) -> [a] -> [[a]].-}

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy xss (x:xs) = (x:ys) : groupBy xss zs where
    (ys, zs) = span (xss x) xs

{-3. Implemente as funcoes all e any, ambas com assinatura (a -> Bool) -> [a] -> Bool, que decidem, respectivamente, se todos ou algum elemento de uma lista satisfazem o
predicado dado como entrada. Perceba que elas podem ser dadas como composicoes de and, or e map.-}

all :: (a -> Bool) -> [a] -> Bool
all x = and . map x

any :: (a -> Bool) -> [a] -> Bool
any x = or . map x


{-4. Escreva a funcao merge :: Ord a => [a] -> [a] -> [a] que, dadas duas listas ordenadas como entrada, retorna uma lista ordenada com todos os elementos das listas
recebidas.-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge lx@(x:xs) ly@(y:ys) = cond (x <= y) (x : merge xs ly) (y : merge lx ys)


{-5. Escreva a funcao split :: [a] -> ([a], [a]) que, dada uma lista xs como entrada, retorna duas listas: a primeira contem os elementos das posicoes ımpares de xs, e a
segunda os elementos das posicoes pares de xs. Aqui, estamos admitindo que a cabeca de uma lista esta na posicao 1.-}
split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:[]) = ([x], [])
split (x:xs) = (x:ys, zs) where
    (zs,ys) = split xs


{-6. Agora, utilizando merge e split, implementadas anteriormente, desenvolva a funcao mergesort :: Ord a => [a] -> [a].-}

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort cs) ( mergesort ds) where
    (cs, ds) = split xs


{-*************************Notas de aula*************************-}

{- Exercı́cio 1 Implemente os operadores (&&) e (||), ambos com assinatura Bool -> Bool-> Bool. 
Esses operadores funcionam de forma análoga a and e or, respectivamente, mas tomam necessariamente dois valores booleanos como argumentos. -}

&& :: Bool -> Bool -> Bool
&& [] = True
&& (True:xs) = && xs
&& (False:_) = False

|| :: Bool -> Bool -> Bool
|| [] = False
|| (True:_) = True
|| (False:xs) = or xs


{- Exercı́cio 2 Implemente a função isInfixOf :: Eq a => [a] -> [a] -> Bool, 
que decide se a primeira lista é sublista contı́nua da segunda. -}
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf (x : xs ) (y: ys ) = cond (x == y) ( isInfixOf xs ys ) False


{- Exercı́cio 4 Implemente a função insert :: Ord a => a -> [a] -> [a] que, dado um elemento de um tipo ordenável e uma lista ordenada, 
insere o elemento na lista, preservando a ordenação. -}

insert :: Ord a => a -> [a]->[a]
insert y [] = [y]
insert y (x:xs) = if y<=x then y:x:xs else x:insert y xs


{- Exercı́cio 5 Implemente a função insertionSort :: Ord a => [a] -> [a], que toma uma
lista de elementos ordenáveis e retorna uma versão ordenada dessa lista. -}

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:[]) = x
insertionSort (x:xs) = insert x (insertionSort xs) 


{- Exercı́cio 6 Implemente a função replicate :: Int -> a -> [a] que, dado um inteiro não-negativo n e um elemento x, cria uma lista de n elementos, todos iguais a x. 
Note que replicate pode ser definida como uma composição de take e repeat, essa última função proposta na Subseção 4.2.-}

replicate :: Int -> a -> [a]
replicate x y = take x (repeat y) 


{- Exercı́cio 7 Implemente a função inits :: [a] -> [[a]], que retorna todos os prefixos da lista dada como entrada. -}

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)


{- Exercı́cio 8 Implemente a função tails :: [a] -> [[a]], que retorna todos os sufixos da lista dada como entrada.-}

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs : tails (tail xs)




