data BTree a = Empty
            | Node a (BTree a) (BTree a)
        deriving Show

arvore = Node 5 (Node 2(Node 1 Empty
                               Empty)
                        (Node 3 Empty
                                Empty))
                (Node 9 (Node 7 (Node 6 Empty
                                        Empty)
                                (Node 8 Empty
                                        Empty))
                        Empty)

altura :: BTree a -> Int
altura Empty = 0
altura (Node e l r) = max (altura l) (altura r) +1

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node e l r) = 1 + contaNodos l + contaNodos r

prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune n (Node e l r) = Node e (prune(n-1) l) (prune(n-1) r)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node e _ _) = [e]
path (h:t) (Node e l r) = e : path t (if h then r else l)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node e l r) = Node e (mirror r) (mirror l)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node e1 l1 r1) (Node e2 l2 r2) = Node (f e1 e2) (zipWithBT f l1 l2) (zipWithBT f r1 r2)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) l r) = (Node a unzipBTL1 unzipBTR1,Node b unzipBTL2 unzipBTR2,Node c unzipBTL3 unzipBTR3)
        where (unzipBTL1,unzipBTL2,unzipBTL3) = unzipBT l
              (unzipBTR1,unzipBTR2,unzipBTR3) = unzipBT r

-- exercicio 2
minimo :: Ord a => BTree a -> a
minimo Empty = error "No bitches"
minimo (Node e Empty _) = e
minimo (Node e l r) = minimo l

semiminimo :: Ord a => BTree a -> BTree a
semiminimo Empty = error "L"
semiminimo (Node e Empty _) = Empty
semiminimo (Node e l r) = Node e (semiminimo l) r

-- exercicio 3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
    | Rep
    | Faltou
    deriving Show
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

inscNum :: Numero -> Turma -> Bool
inscNum n1 Empty = False
inscNum n1 (Node (n2,_,_,_) l r) | n1 == n2 = True
                                 | otherwise = inscNum n1 (if n1 < n2 then l else r)

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome no1 (Node (_,no2,_,_) l r) | no1 == no2 = True
                                    | otherwise = inscNome no1 l || inscNome no1 r

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (n,no,TE,clas) l r) = trabEst l ++ [(n,no)] ++ trabEst r
trabEst (Node _ l r) = trabEst l ++ trabEst r

nota :: Numero -> Turma -> Maybe Classificacao
nota n (Node (n1,_,_,clas) l r) | n == n1 = Just clas
                                | n < n1 = nota n l
                                | otherwise = nota n r
nota _ _ = Nothing

percFaltas :: Turma -> Float
percFaltas turma = sumFaltas turma / numAlunos turma * 100
        where sumFaltas :: Turma -> Float
              sumFaltas Empty = 0
              sumFaltas (Node (_,_,_,Faltou) l r) = 1 + sumFaltas l + sumFaltas r
              sumFaltas (Node _ l r) = sumFaltas l + sumFaltas r
              numAlunos = fromIntegral . contaNodos