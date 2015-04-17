data BExpr = F | T | Not BExpr
	       | BExpr :&: BExpr
 	       | BExpr :|: BExpr

eval :: BExpr -> Bool
eval T = True
eval F = False
eval (Not b) = not (eval b)
eval (b1 :&: b2) = eval b1 && eval b2
eval (b1 :|: b2) = eval b1 || eval b2


data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

bfs :: Tree a -> [a]
bfs x = traverse [x]

traverse :: [Tree a] -> [a]
traverse [] = []
traverse ts = rootlabels ++ traverse (concat children)
	where rootlabels = [ x | Node x _ _ <- ts ]
	      children = [ [x]++[y] | Node _ x y <- ts]

t = Node 1 (Node 10 Empty (Node 16 Empty Empty)) (Node 17 (Node 14 Empty Empty) (Node 20 Empty Empty))


data Edit = Change Char | Copy | Delete | Insert Char deriving (Eq, Show)

transform:: String -> String -> [ Edit ]
transform [] [] = []
transform st [] = replicate (length st) Delete
transform [] st = map Insert st
transform (a:x) (b:y) | a==b = Copy : transform x y
		      | otherwise = best [ Delete   : transform x (b:y) ,
		                           Insert b : transform (a:x) y ,
					   Change b : transform x y ]

cost :: [Edit] -> Int
cost = length . filter (/=Copy)

best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs) | cost x <= cost b = x
	    | otherwise = b
	      where b = best xs
