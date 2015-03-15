q1f1b :: [Int] -> [Int]
q1f1b [] = []
q1f1b lt = [(*3) elem | elem <- lt, elem>=3, elem<=10] 
