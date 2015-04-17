data Edit = Change Char | Copy | Delete | Insert Char deriving (Eq, Show)

transform :: String -> String -> [Edit]
transform [] [] = []
transform st [] = replicate (length st) Delete
transform [] st = map Insert st
transform (a:x) (b:y)
	| a==b = Copy : transform x y
	| otherwise = best [ Delete : transform x (b:y), Insert b : transform (a:x) y, Change b : transform x y ]

cost :: [Edit] -> Int
cost = length . filter (/=Copy)

best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
	| cost x <= cost b = x
	| otherwise = b
		where b = best xs
