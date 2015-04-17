rev2 :: [a] -> [a]
rev2 [x, y] = y:[x]
rev2 (x:xs) = x:xs

--Method2:
{-
rev2 :: [a] -> [a]
rev2 lt = 
	if leng lt == 2
	then reverse lt
	else lt

leng :: [a] -> Int
leng lt =
	if null lt
	then 0
	else 1 + (leng $ tail lt)
-}
