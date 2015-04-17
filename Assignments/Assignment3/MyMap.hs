myMap :: (a->b)->[a]->[b]
myMap f = foldr (\x -> \xs -> f x : xs) []
{-
Method2:
myMap f = foldr (:) [] (map f )
-}
