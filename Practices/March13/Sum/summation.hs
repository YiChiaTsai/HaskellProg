summ :: [Int] -> Int
summ lt = 
	if null lt
	then 0
	else (head lt) + (summ $ tail lt)

main = do
	putStrLn $ show $ summ []
	putStrLn $ show $ summ [1, 2]
	putStrLn $ show $ summ [3, 4, 5]
