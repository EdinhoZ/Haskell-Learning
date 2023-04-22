--ex 1
pares :: [a] -> [a]
pares [] = []
pares [x] = [x]
pares (h:i:t) = h : pares t
----------------- ou ------------------
pares' :: [a] -> [a]
pares' [] = []
pares' (h:t) = h : impares' t

impares' :: [a] -> [a]
impares' [] = []
impares' (h:t) = pares' t
--------------------------------------

--ex 2
impares :: [a] -> [a]
impares [] = []
impares [x] = [x]
impares (h:i:t) = i : impares t 

-- catMaybes
catmaybes :: [Maybe a] -> [a]
catmaybes [] = []
catmaybes (x:xs) = case x of 
    Just x -> x : catmaybes xs
    Nothing -> catmaybes xs

-- catMaybes ordem superior
catmaybesOs :: [Maybe a] -> [a]
catmaybesOs l = foldr (\x acc -> case x of Just x -> x : acc; Nothing -> acc) [] l

