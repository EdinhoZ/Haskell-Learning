--ex1
-- a)
-- funA [2,3,5,1]
-- = funA (2:[3,5,1])
-- = 2^2 + funA [3,5,1]
-- 4 + funA [3:(5,1)]
-- 4 + 3^2 + funA [5,1]
-- 4 + 9 + 5^2 + funA [1]
-- 4 + 9 + 25 + 1 + 0
-- = 39

-- b)
-- funB [8,5,12]
-- = funB (8:[5,12]) | mod 8 2 == 0
-- = 8 : funB [5,12] | mod 5 2 =/ 0
-- = 8 : (funB [12]) | mod 12 2 == 0
-- = 8 : (12 : funB [])
-- = 8 : 12 : [] = [8,12]

--ex2

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h * 2 : dobros t

num0corre :: Char -> String -> Int
num0corre c "" = 0
num0corre c (h:t)
    | c == h = 1 + num0corre c t
    | otherwise = num0corre c t


positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t)
    | h > 0 = positivos t
    | otherwise = False

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if (h > 0) then h : (soPos t) else (soPos t)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((_,h2):t) = h2 : segundos t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+ra,b+rb,c+rc)
    where (ra,rb,rc) = sumTriplos t

--ex4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((c,g):t) = if g == n
                    then 1 + conta n t
                    else conta n t

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,g):t) = (c * fromIntegral g,g-1) : deriv t

mult :: Monomio -> Polinomio -> Polinomio
mult n [] = []
mult (a,b) ((c,d):t) = (a*c,b+d) : mult (a,b) t

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,g):t) = inserir (c,g) (normaliza t)

inserir :: Monomio -> Polinomio -> Polinomio
inserir m [] = [m]
inserir (cm,gm) ((c,g):t)
    | g == gm = (c+cm,g) : t
    | otherwise = (c,g) : inserir (cm,gm) t