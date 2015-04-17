q1f1a :: [Int] -> [Int]
q1f1a [] = []
q1f1a lt = map (*3) $ filter (<=10) $ filter (>=3) lt
{-
Method2:
q1f1a lt = do
	let lt2 = filter (>=3) lt
	let lt3 = filter (<=10) lt2
	map (*3) lt3
-}
