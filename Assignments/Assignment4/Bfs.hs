data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

bfs :: Tree a -> [a]
bfs x = traverse [x]

traverse :: [Tree a] -> [a]
traverse [] = []
traverse ts = rootlabels ++ traverse children
	where rootlabels = [ x | Node x _ _ <- ts ]
	      children = concat [ [y] ++ [z] | Node _ y z <- ts]
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

createTree1 = Node 1 
	(Node 10 
		Empty (Node 16 Empty Empty)
	)
	(Node 17 
		(Node 14 Empty Empty) 
		(Node 20 Empty Empty)
	)
createTree2 = Node 'A'
	(Node 'B'
		(Node 'C' Empty Empty)
	 	(Node 'D' Empty Empty)
	)
	(Node 'E'
		(Node 'F' Empty Empty)
		(Node 'G' 
			Empty 
			(Node 'H'
				(Node 'I' Empty Empty)
				Empty
			)
		)
	)
