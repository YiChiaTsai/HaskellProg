fold :: (Int -> Int -> Int) -> [Int] -> Int
fold fn lt =
	if null $ tail lt
	then head lt
	else fn (head lt) (fold fn $ tail lt)
