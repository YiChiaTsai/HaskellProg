leng :: [a] -> Int
leng lt =
	if null lt
	then 0
	else 1 + (leng $ tail lt)

rev2 :: [a] -> [a]
rev2 lt = 
	if leng lt == 2
	then reverse lt
	else lt
