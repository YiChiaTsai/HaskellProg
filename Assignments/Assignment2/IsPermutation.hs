removeOnce :: Int->[Int]->[Int]
removeOnce x [] = []
removeOnce x lt = 
	if head lt /= x
	then head lt : removeOnce x (tail lt)
	else tail lt

isPermutation :: [Int] -> [Int] -> Bool
isPermutation [] [] = True
isPermutation [_] [] = False
isPermutation [] [_] = False
isPermutation lt1 lt2 = do
	let lt3 = removeOnce (head lt1) lt2
	if lt3 == lt2
	then False
	else isPermutation (tail lt1) lt3
