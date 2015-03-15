power1 :: Int -> Int -> Int
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product $ replicate k n
