pairs :: [Int] -> [(Int, Int)]
pairs lt = do
	let x = [0] ++ lt ++ [0]
	zip (init x) (tail x)

pascal :: Int -> [Int]
pascal 1 = [1]
pascal n = [a+b | (a,b) <- pairs (pascal (n-1))]
