tailUpto :: Int->Int->[Int]->[Int]
tailUpto m n lt
	| m>n = lt
	| otherwise = tailUpto m (n-1) (n:lt)
--Method 2:
--tailUpto m n lt = [m..n] ++ lt

upto :: Int->Int->[Int]
upto m n = tailUpto m n []
