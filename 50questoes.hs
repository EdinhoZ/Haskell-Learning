module Questoes50 where
import Data.List (delete)

enumFromTo' :: Int -> Int ->[Int]
enumFromTo' x y | x > y = []
                | otherwise = x : enumFromTo' (x+1) y

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | x > z && y >= z || x < z && y < z = []
                      | otherwise = x : enumFromThenTo' y (2*y - x) z

plusPlus :: [a] -> [a] -> [a]
plusPlus [] l = l
plusPlus (x:xs) l = x : plusPlus xs l

posicao :: [a] -> Int -> a
posicao (x:_) 0 = x
posicao (_:xs) n = posicao xs (n-1)

reverte :: [a] -> [a]
reverte [] = []
reverte (x:xs) = reverte xs ++ [x]

pega :: Int -> [a] -> [a]
pega _ [] = []
pega n (x:xs) | n <= 0 = []
              | otherwise = x : pega (n-1) xs

tira :: Int -> [a] -> [a]
tira _ [] = []
tira n (x:xs) | n <= 0 = x : xs
              | otherwise = tira (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

replica :: Int -> a -> [a]
replica 0 _ = []
replica n x | n < 0 = []
            | otherwise = x : replica (n-1) x

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse n (x:xs) = x : n : intersperse n xs

group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (x:xs) | elem x (head r) = (x:(head r)) : tail r
             | otherwise = [x] : r
             where r = group xs

concat' :: [[a]] -> [a]
concat' [[]] = []
concat' (x:xs) = x ++ concat' xs

inits :: [a] -> [[a]]
inits [] = []
inits l = inits (init l) ++  [l]

tails :: [a] -> [[a]]
tails [] = []
tails l = l : tails (tail l)

heads :: [[a]] -> [a]
heads [] = []
heads ([]:xs) = heads xs
heads ((x:y):xs) = x : heads xs

total :: [[a]] -> Int
total [] = 0
total (x:xs) = length x + total xs

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((x,y,z):xs) = (x,z) : fun xs

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):xs) =  a ++ cola xs

idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade ano idade' ((x,y):xs) | (ano - y) >= idade' = x : idade ano idade' xs
                            | otherwise = idade ano idade' xs

powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 1 = [1]
powerEnumFrom n m | m > 1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
                  | otherwise = []

isPrime :: Int -> Bool
isPrime n = n >= 2 && primeCheck n 2

primeCheck :: Int -> Int -> Bool
primeCheck n m | m*m > n = True
               | mod n m == 0 = False
               | otherwise = primeCheck n (m+1)
            
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l l' = l == l' || isSuffixOf l xs
        where (x:xs) = l'

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) = x == y && isSubsequenceOf xs ys || isSubsequenceOf (x:xs) ys

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n l = elemIndicesAux n l 0
        where elemIndicesAux _ [] _ = []
              elemIndicesAux n (x:xs) i | n == x = i : elemIndicesAux n xs (i+1)
                                        | otherwise = elemIndicesAux n xs (i+1) 

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) | x `elem` xs = nub' xs
            | otherwise = x : nub' xs

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (x:xs) | n == x = xs
                 | otherwise = x : delete' n xs

remove :: Eq a => [a] -> [a] -> [a]
remove l [] = l
remove [] _ = []
remove l (x:xs) = remove (delete x l) xs

union' :: Eq a => [a] -> [a] -> [a]
union' (x:xs) [] = (x:xs)
union' [] _ = []
union' (x:xs) (y:ys) | x == y = x : union' xs ys
                     | otherwise = x : y : union' xs ys

pMaior :: Ord a => [a] -> Int
pMaior [] = error "lista vazia"
pMaior [x] = 0
pMaior (x:xs) | x >= maiorE = 0
              | otherwise = maiorT + 1
        where maiorT = pMaior xs
              maiorE = xs !! maiorT