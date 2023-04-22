import Data.Char (isDigit, isAlpha, intToDigit)

--ex 1
digitAlpha :: String -> (String,String)
digitAlpha "" = ("", "")
digitAlpha (h:t)
    | isAlpha h = (h : alphas, digits)
    | isDigit h = (alphas , h : digits)
    | otherwise = (alphas,digits)
    where (alphas,digits) = digitAlpha t

--exemplo acumulador--------------
soma :: [Int] -> Int
soma l = somaAux 0 l

somaAux :: Int -> [Int] -> Int
somaAux acc [] = acc
somaAux acc (h:t) = somaAux (acc + h) t
----------------------------------

--ex 4
fromDigits :: [Int] -> Int
fromDigits l = fromDigitsAux l 0

fromDigitsAux :: [Int] -> Int -> Int
fromDigitsAux [] acc = acc
fromDigitsAux (h:t) acc = fromDigitsAux t (h + 10 * acc)

--ex7 (transforma 382 em "382")
intToStr :: Int -> String
intToStr 0 = "0"
intToStr n = intToStrAux "" n

intToStrAux :: String -> Int -> String
intToStrAux acc 0 = acc
intToStrAux acc n = intToStrAux (intToDigit r : acc) q
    where q = n `div` 10
          r = n `mod` 10

--ex 8
--a)
--[6,12,18]
a = [x | x <- [1..20], mod x 6 == 0]
--b)
--[6,12,18]
b = [x | x <- [1..20], mod x 6 == 0]
--c)
--[(20,10),(19,11),(18,12),(17,13),(16,14),(15,15),(14,16),(13,17),(12,18),(11,19),(10,20)]
c = [(30 - y,y) | y <- [10..20]]
--d)
--[1,1,4,4,9,9,16,16,25,25]
d = [x^2 | x <- [1..5], _ <- [1..2]]

--ex 9
--d)
d9 = [take n [1,1..] | n <- [1..5]]