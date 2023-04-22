module Ficha8 where

data Frac = F Integer Integer

normaliza :: Frac -> Frac
normaliza (F x y) = F (s*a) b
    where d = mdc (abs x) (abs y)
          a = div (abs x) d
          b = div (abs y) d
          s = signum x * signum y

mdc :: Integer -> Integer -> Integer
mdc x y | x == y = x
        | x > y = mdc (x-y) y
        | otherwise = mdc x (y-x)

instance Eq Frac where
    f1 == f2 = (a == x) && (b == y)
        where (F a b) = normaliza f1
              (F x y) = normaliza f2

instance Ord Frac where
    f1 <= f2 = a*y <= x*b
        where (F a b) = normaliza f1
              (F x y) = normaliza f2

instance Show Frac where
    show :: Frac -> String
    show (F a b) = "("++(show a)++"/"++(show b)++")"

instance Num Frac where
    (F a b) + (F x y) = F (a*y + b*x) (b*y)
    (F a b) * (F x y) = F (a*x) (b*y)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) = F(signum a*signum b) 1
    fromInteger n = F n 1

maioresDobro :: Frac -> [Frac] -> [Frac]
maioresDobro f = filter (>(2*f))