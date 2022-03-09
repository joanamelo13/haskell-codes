module Main where
import Prelude (String, Eq((==) ,(/=)), Int, Num((+), (-), (>=), Char, IO, Ord, readFile, putStrLn, Show(show), Monad(>>=), pure, undefined, otherwise)
import Bool
import Functions 
import List
import Maybe
import qualified BSTree as BST
import qualified Graph as G

--Joana Maria Chaves Melo - 415604
--Criação da BSTree
data BSTree p q = Empty
                | Branch (p, q) (BSTree p q) (BSTree p q)

empty :: BSTree p q
empty = Empty
------------------------------------------------------

--Questão 6 do Lab 4
--Função main e leitura do arquivo heyjude.txt
main :: IO ()
main = readFile " heyjude.txt " >>= putStrLn
------------------------------------------------------
-- Laboratórios

-- Lab 4

--Questão 1
remove' :: Eq a => a -> [a] -> [a]
remove' x = filter (/=x)

--Questão 2
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x [y] = cond (x == y) [] [[y]]
split x ys = ps : split x qs where
    (ps, qs) = f ys
    f = span (/=x) . dropWhile (==x)

--Questão 3
lines :: [Char] -> [[Char]]
lines = split '\n'

words :: [Char] -> [[Char]]
words = split ' '

--Questão 4
count :: [[Char]] ->BST.BSTree [Char] Int
count xs = foldl f BST.empty xs where 
	f btree plv= if (BST.contains btree plv)
		then BST.update btree plv (+1)                                                                       
		else BST.insert btree (plv,1)

--Questão 5
process :: [Char] -> [Char]
process = makeOutput . countWords . clean where
    clean = undefined
    countWords = undefined
    makeOutput = concat . intercalate " \n" . map f . BST.inOrder
    f ( str , c ) = str ++ ": " ++ show c

countWords :: [String] -> BST.BSTree [String] Int
countWords xs = foldl f BST.empty xs where 
	f btree plv= if (BST.contains btree plv)
		then BST.update btree plv (+1)                                                                       
		else BST.insert btree (plv,1)

clean :: [Char] -> [Char]
clean xs = foldr remove xs [',','!','?',')','(']

--Lab 5

--sortBy e mergesort
merge' :: Ord a => (a -> a-> Bool) -> [a] -> [a] -> [a]
merge' c [] ys = ys
merge' c xs [] = xs
merge' c l1@(x:xs) l2@(y:ys) = cond (c x y) (x : merge' c xs l2) (y : merge' c l1 ys)

mergesort' :: Ord a => (a -> a-> Bool)-> [a] -> [a]
mergesort' c [] = []
mergesort' c [x] = [x]
mergesort' c xs = merge' c (mergesort' c ps) ( mergesort' c qs) where
    (ps, qs) = split xs

--sortBy :: Ord a => (a -> a-> Bool)-> [a] -> [a]
--sortBy cmp . BST.inOrder
--sortBy cpm (x:xs) = merge' (sortBy f ps) (sortBy f qs) where
    --(ps, qs) = split xs

-----------------------------------------------------
--Notas de aula

-- Questão 14
leaves :: BSTree p q -> Int
leaves  Empty = 0
leaves (Branch a Empty Empty) = 1
leaves (Branch a left  right) = leaves left + leaves right + 0

-- Questão 15
inOrder :: BSTree p q -> [(p, q)]
inOrder Empty = []
inOrder (Branch n left right) = inOrder left ++ (n : inOrder right)

preOrder :: BSTree p q -> [(p, q)]
preOrder  Empty = []
preOrder  (Branch n left right) = n:preOrder  left ++ preOrder  right

postOrder::BSTree p q -> [(p, q)]
postOrder Empty = []
postOrder (Branch n left right) = postOrder left ++ postOrder right ++ [n]

--Questão 16
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x: _) !? 0 = Just x
(_: xs ) !? k = xs !? (k - 1)