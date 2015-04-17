raise :: Int -> Int -> Int
raise n k = product [(*n) elem | elem <- take k $ [1, 1 ..]]
