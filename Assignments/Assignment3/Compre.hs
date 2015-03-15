compre :: [a]->(a->b)->(a->Bool)->[b]
compre xs f p = do
	let lt = filter p xs
	map f lt
