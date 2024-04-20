ssq :: Int -> Int -> Int
ssq x y = x*x + y*y

len :: [a] -> Integer
len = foldr (\ x -> (+) 1) 0

appendL :: [a] -> [a] -> [a]
appendL (x:xs) (y:ys) = foldr (:) (x:xs) (y:ys)
