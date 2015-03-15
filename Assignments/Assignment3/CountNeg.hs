countNeg :: [Int] -> Int
countNeg lt = sum [1 | elem <- lt, elem<0]
