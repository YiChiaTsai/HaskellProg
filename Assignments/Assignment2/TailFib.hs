tailFib :: Int->Int->Int->Int
tailFib 0 0 1 = 0
tailFib 1 0 1 = 1
tailFib n acc1 acc2
	| n==2 = acc1+acc2
	| n>2 = tailFib (n-1) acc2 (acc1+acc2)

fib :: Int -> Int
fib n = tailFib n 0 1
