q1f1 :: [Int] -> [Int]
q1f1 [] = []
q1f1 (x:xs) | x < 3 = q1f1 xs
			| x > 10 = q1f1 xs
			| otherwise = x*3 : q1f1 xs
