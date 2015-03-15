raise :: Int -> Int -> Int
raise n k = product [(*k) elem | elem <- take n $ [1, 1 ..]]
