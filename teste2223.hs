import Data.List
-- ex1
unlines' :: [String] -> String
unlines' [] = ""
unlines' [x] = x
unlines' (h:t) = h ++ "\n" ++ unlines' t

--ex 2
-- a)
type Mat = [[Int]]
stringToMat :: String -> Mat
stringToMat s = map stringToVector (lines s)

stringToVector :: String -> [Int]
stringToVector s = map read (words (map (\c -> if c == ',' then ' ' else c) s))

-- b)
transposta :: String -> Mat
transposta s = undefined

-- ex3
-- a)
data Lista a = Esq a (Lista a) | Dir (Lista a) a | Nula
semUltimo :: Lista a -> Lista a
semUltimo (Esq a Nula) = Nula
semUltimo (Esq a l) = Esq a (semUltimo l)
semUltimo (Dir l a) = l

-- b)
instance Show a => Show (Lista a) where
    show l = "[" ++ showAux l ++ "]"
        where
            showAux Nula = ""
            showAux (Esq a Nula) = show a
            showAux (Esq a l) = show a ++ "," ++ showAux l
            showAux (Dir Nula a) = show a
            showAux (Dir l a) = showAux l ++ "," ++ show a