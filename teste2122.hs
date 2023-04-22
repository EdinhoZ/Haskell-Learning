import Data.List (intercalate)

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (h:t) (x:xs) = (h,x) : zip' t xs

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:s:t) | h < s = h : preCrescente (s:t)
                     | otherwise = [h]

amplitude :: [Int] -> Int
amplitude l = uncurry (flip (-)) . foldr (\x (acc_min,acc_max) -> (min x acc_min, max x acc_max)) (head l, head l) $ l

type Mat a = [[a]]
soma :: Num a => Mat a -> Mat a -> Mat a
soma = zipWith . zipWith $ (+)

type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda

instance Show Agenda where
    show Vazia = ""
    show (Nodo (nome, tlfs) l r) = 
        show l
        ++ nome ++ " " ++ intercalate "/" (map show tlfs) ++ "\n"
        ++ show r
