power2 :: Int -> Int -> Int
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k = 
	if even k 
	then power2 (n^2) (div k 2)
	else n * power2 n (k-1)
