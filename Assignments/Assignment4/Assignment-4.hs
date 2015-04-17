--eval
eval :: BExpr -> Bool
data BExpr = F | T | Not BExpr
			| BExpr :&: BExpr
			| BExpr :|: BExpr

eval T = True
eval F = False
eval (Not x) = not (eval x)
eval (x :&: y) = eval x && eval y
eval (x :|: y) = eval x || eval y
--eval-End

--bfs
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

bfs :: Tree a -> [a]
bfs x = traverse [x]

traverse :: [Tree a] -> [a]
traverse [] = []
traverse ts = rootlabels ++ traverse children
	where rootlabels = [ x | Node x _ _ <- ts  ]
	      children = concat [ [y] ++ [z] | Node _ y z <- ts ]
{-
Method2:
	bfs :: Tree a -> [a]
		bfs tree = tbf [tree]
		where
		tbf [] = []
		tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs))
		nodeValue (Node a _ _) = a
		leftAndRightNodes (Node _ Empty Empty) = []
		leftAndRightNodes (Node _ Empty b)     = [b]
		leftAndRightNodes (Node _ a Empty)     = [a]
		leftAndRightNodes (Node _ a b)         = [a,b]
-}
--bfs-End

--transform
data Edit = Change Char | Copy | Delete | Insert Char deriving (Eq, Show)

transform :: String -> String -> [Edit]
transform [] [] = []
transform st [] = replicate (length st) Delete
transform [] st = map Insert st
transform (a:x) (b:y)
	| a==b = Copy : transform x y
	| otherwise = best [ Delete : transform x (b:y), Insert b : transform (a:x) y, Change b : transform x y  ]

cost :: [Edit] -> Int
cost = length . filter (/=Copy)

best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
	| cost x <= cost b = x
	| otherwise = b
		where b = best xs
--transform-End


main = do
    print $ eval (F :|: ((T :&: T) :|: F)) -- True
    print $ eval (Not (T :&: ((T :&: Not F) :|: F))) -- False
    print $ bfs (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 (Node 6 Empty Empty) (Node 7 Empty Empty))) -- [1,2,3,4,5,6,7]
    print $ bfs (Node 1 (Node 10 Empty (Node 16 Empty Empty)) (Node 17 (Node 14 Empty (Node 3 Empty Empty)) (Node 20 Empty Empty))) -- [1,10,17,16,14,20,3]
    print $ bfs (Node 'A' (Node 'B' (Node 'C' Empty Empty) (Node 'D' Empty Empty)) (Node 'E' (Node 'F' Empty Empty) (Node 'G' Empty (Node 'H' (Node 'I' Empty Empty) Empty)) ) ) -- "ABECDFGHI"
    print $ transform "125637" "126537" -- [Copy,Copy,Delete,Copy,Insert '5',Copy,Copy]
    print $ transform "abcedgfhi" "abdefghi" -- [Copy,Copy,Change 'd',Copy,Change 'f',Copy,Delete,Copy,Copy]
    print $ transform "today" "toady" -- [Copy,Copy,Delete,Copy,Insert 'd',Copy]
    print $ transform "abcde" "bbc" -- [Change 'b',Copy,Copy,Delete,Delete]
    print $ transform "fish" "chips" -- [Insert 'c',Change 'h',Copy,Insert 'p',Copy,Delete]
    print $ transform "1234" "4321" -- [Delete,Change '4',Copy,Insert '2',Change '1']
    print $ transform "123456" "654321" -- [Delete,Change '6',Change '5',Copy,Insert '3',Change '2',Change '1']
    print $ transform "12345678" "87654321" -- [Delete,Change '8',Change '7',Change '6',Copy,Insert '4',Change '3',Change '2',Change '1']
    print $ transform "constructure" "substructure" -- [Change 's',Change 'u',Change 'b',Copy,Copy,Copy,Copy,Copy,Copy,Copy,Copy,Copy]
