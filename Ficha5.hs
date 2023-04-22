import Data.List (nub)
--ex1
--a)
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = True
any' f (x:xs)
    | f x = True
    | otherwise = any' f xs

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f a [] = []
deleteBy' f a (x:xs) | f a x = xs
                     | otherwise = x : deleteBy' f a xs

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (h:t) (x:xs) = f h x : zipWith' f t xs
zipWith' _ _ _ = []

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (h:t) | f h = h : takeWhile' f t
                   | otherwise = []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t) | f h = dropWhile' f t
                   | otherwise = h : t

span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f (h:t) | f h = (h : takeWhile' f t,dropWhile' f t)
              | otherwise = ([],h:t)

--exercício 2
type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\x -> snd x == n) p

conta :: Int -> Polinomio -> Int
conta n p = length $ selgrau n p

grau :: Polinomio -> Int
grau p = maximum $ map snd p

deriv :: Polinomio -> Polinomio
deriv p = map (\(c,g) -> (c * fromIntegral g, g - 1)) $ filter (\(c,g) -> g /= 0) p

deriv' :: Polinomio -> Polinomio
deriv' p = [ (c * fromIntegral g, g - 1) | (c,g) <- p, g /= 0]

calcula :: Float -> Polinomio -> Float
calcula n p = foldl (\acc (c,g) ->acc + c*(n^g)) 0 p

simp :: Polinomio -> Polinomio
simp = filter (\(c,g) -> c /= 0)

-- ex3
type Mat a = [[a]]

-- a)
dimOK :: Mat a -> Bool
dimOK = (== 1) . length . nub . map length

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat m = (length m, length (head m))