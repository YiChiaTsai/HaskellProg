fibonacciLt1 :: Int -> [Int]
fibonacciLt1 n =
    [fibonacci x | x <- [0 .. n]]
    where fibonacci 0 = 0
          fibonacci 1 = 1
          fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
