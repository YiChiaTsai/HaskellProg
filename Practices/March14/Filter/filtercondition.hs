--Important type concept
filtercond :: (Int -> Bool) -> [Int] -> [Int]
filtercond cond lt = 
	if null lt then lt
	else
		if cond $ head lt then head lt : (filtercond cond $ tail lt)
		else filtercond cond $ tail lt
