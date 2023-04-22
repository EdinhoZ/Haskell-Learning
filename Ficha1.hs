module Ficha1 where

--ex1

perimetro :: Double -> Double
perimetro raio = 2*pi*raio

dist :: (Double,Double) -> (Double,Double) -> Double
dist (xa,ya) (xb,yb) = sqrt ((xa - xb)^2 + (ya - yb)^2)

primUlt :: [a] -> (a,a)
primUlt lista = (head lista,last lista)

multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

truncaImpar :: [a] -> [a]
truncaImpar lista = if not (multiplo (length lista) 2) 
                    then tail lista 
                    else lista

max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b

max3 :: Int -> Int -> Int -> Int
max3 a b c = if max2 a b == a then max2 a c else max2 b c

--ex2

nRaizes :: Float -> Float-> Float -> Int
nRaizes a b c
    | b^2 - 4*a*c < 0 = 0
    | b^2 - 4*a*c == 0 = 1
    | otherwise = 2

raizes :: Float -> Float -> Float -> [Float]
raizes a b c
    | n == 1 = [(-b + sqrt (b^2 - 4*a*c)) / (2*a)]
    | n == 2 = [(-b + sqrt (b^2 - 4*a*c)) / (2*a), (-b - sqrt (b^2 - 4*a*c)) / (2*a)]
    |otherwise = []
    where
        n = nRaizes a b c

--ex3 

type Hora = (Int,Int)
tempo :: Hora -> Bool
tempo (h,m) = h>=0 && h <= 23 && m >= 0 && m <= 59

tmHora :: Hora -> Hora -> Bool
tmHora (h1,m1) (h2,m2) = h1 > h2 || h1 == h2 && m1 > m2

converH :: Hora -> Int
converH (h,m) = (h*60) + m

converM :: Int -> Hora
converM min = (div min 60,mod min 60)

difH :: Hora -> Hora -> Int
difH (h1,m1) (h2,m2) = (h1 - h2)*60 + (m1 - m2)

addM :: Hora -> Int -> Hora
addM (h,m1) m2 = (h + hr,mr)
    where
        (hr,mr) = converM (m1 + m2)

--ex4

data Hora = H Int Int deriving Show

tempo :: Hora -> Bool
tempo (H h m) = h>=0 && h <= 23 && m >= 0 && m <= 59

tmHora :: Hora -> Hora -> Bool
tmHora (H h1 m1) (H h2 m2) = h1 > h2 || h1 == h2 && m1 > m2

converH :: Hora -> Int
converH (H h m) = (h*60) + m

converM :: Int -> Hora
converM min = H h m 
    where (h,m) = (div min 60,mod min 60)

difH :: Hora -> Hora -> Int
difH (H h1 m1) (H h2 m2) = (h1 - h2)*60 + (m1 - m2)

addM :: Hora -> Int -> Hora
addM (H h m1) m2 = H (h + hr) mr
    where
        (H hr mr) = converM (m1 + m2)

--ex5 

data Semaforo = Verde | Amarelo | Vermelho deriving (Show ,Eq)

next :: Semaforo -> Semaforo
next Amarelo = Vermelho
next Vermelho = Verde
next Verde = Amarelo

stop :: Semaforo -> Bool
stop Verde = False
stop Vermelho = True
stop Amarelo = True

safe :: Semaforo -> Semaforo -> Bool
safe Vermelho _ = True
safe _ Vermelho = True
safe _ _ = False

--ex6

import Distribution.Simple.Utils (xargs)
import Data.Graph (vertices)
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar r a) = r

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar r a) = a

dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt ((x'- x)^2 + (y' - y)^2)
    where
        x = posx p1
        y = posy p1
        x'= posx p2
        y'= posy p2

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono (Retangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2
poligono (Triangulo p1 p2 p3) = posx p1 /= posx p2 || posx p2 /= posx p3 || posx p1 /= posx p3
                                && posy p1 /= posy p2 || posy p2 /= posy p3 || posy p1 /= posy p3

vertices :: Figura ->[Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo p1 p2) = [p1, Cartesiano (posx p1) (posy p2), p2, Cartesiano (posx p2) (posy p1)]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist p1 p2 
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 --semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) --formula de Heron
area (Retangulo p1 p2) = abs(posx p2 - posx p1) * abs(posy p2 - posy p1)
area (Circulo _ r) = pi * (r^2)
