tailFib :: Int->Int->Int->Int
tailFib 0 0 1 = 0
tailFib 1 0 1 = 1
tailFib n 0 1 = tailFib (n-1) 0 1 + tailFib (n-2) 0 1

fib :: Int -> Int
fib n = tailFib n 0 1
