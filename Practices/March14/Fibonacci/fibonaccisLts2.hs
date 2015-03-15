fibonacciLt2 :: Int -> [Int]
fibonacciLt2 n =
    let fibonacci 0 = 0
        fibonacci 1 = 1
        fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
    in [fibonacci x | x <- [0 .. n]]
