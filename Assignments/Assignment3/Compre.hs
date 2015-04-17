compre :: [a]->(a->b)->(a->Bool)->[b]
compre xs f p = map f $ filter p xs
{-
Method2:
compre xs f p = do
	let lt = filter p xs
	map f lt
-}
