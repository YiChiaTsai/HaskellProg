fibonacci1 :: Int -> Int
fibonacci1 n =
	if n==0 || n==1
	then n
	else fibonacci1 (n-1) + fibonacci1 (n-2)
