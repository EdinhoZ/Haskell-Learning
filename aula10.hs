module Aula10 where

data Ponto = P Float Float

--classes e tipos

instance Eq Ponto where
    P x1 y1 == P x2 y2 = x1 == x2 && y1 == y2

instance Ord Ponto where
    P x1 y1 <= P x2 y2 = sqrt (x1^2 + y1^2) <= sqrt (x2^2 + y2^2)

instance Show Ponto where
    show (P x y) = "(" ++ show x ++ "," ++ show y ++ ")"

{- instance Num Ponto where
    P x1 y1 + P x2 y2 = P (x1+x2) (y1+y2) -}

--ex do codeboard
data Genero = Masculino | Feminino | NaoBinario
    deriving Show

data Pessoa = Pessoa { nome :: String, idade :: Int, cc :: Integer, genero :: Genero }

instance Eq Pessoa where
    (==) :: Pessoa -> Pessoa -> Bool
    (==) (Pessoa nome1 idade1 cc1 genero1) (Pessoa nome2 idade2 cc2 genero2) = cc1 == cc2
    
instance Ord Pessoa where
    compare :: Pessoa -> Pessoa -> Ordering
    (Pessoa nome1 idade1 cc1 genero1) `compare` (Pessoa nome2 idade2 cc2 genero2) = idade1 `compare` idade2

instance Show Pessoa where
    show :: Pessoa -> String
    show (Pessoa nome idade cc genero) = "Nome: " ++ nome ++ "\n" ++ "Idade: " ++ show idade ++ "\n" ++ "CC: " ++ show cc ++ "\n" ++ "Género: " ++ show genero