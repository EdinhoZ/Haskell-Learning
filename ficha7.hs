data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a
soma (R e []) = e
soma (R e es) = e + sum (map soma es)

altura :: RTree a -> Int
altura (R e []) = 1
altura (R e es) = 1 + maximum (map altura es)