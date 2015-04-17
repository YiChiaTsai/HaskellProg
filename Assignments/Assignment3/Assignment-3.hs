import Data.List

--countNeg
countNeg :: [Int] -> Int
countNeg lt = sum [1 | elem <- lt, elem<0]
--countNeg-End

--raise
raise :: Int -> Int -> Int
raise n k = product [(*n) elem | elem <- take k $ [1, 1 ..]]
--raise-End

--pairs
pairs :: [Int] -> [(Int, Int)]
pairs lt = do
	let x = [0] ++ lt ++ [0]
	zip (init x) (tail x)
--pairs-End

--pascal
pascal :: Int -> [Int]
pascal 1 = [1]
pascal n = [a+b | (a,b) <- pairs (pascal (n-1))]
--pascal-End

--q1f1a
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
--q1f1a-End

--q1f1b
q1f1b :: [Int] -> [Int]
q1f1b [] = []
q1f1b lt = [(*3) elem | elem <- lt, elem>=3, elem<=10]
--q1f1b-End

--compre
compre :: [a]->(a->b)->(a->Bool)->[b]
compre xs f p = map f $ filter p xs
{-
Method2:
compre xs f p = do
	let lt = filter p xs
	map f lt
-}
--compre-End

--import Data.List (tails)
--subsets
subsets :: [Int] -> [[Int]]
subsets lt = foldr (++) [] [subsetsl elem lt | elem <- [0..(length lt)] ]

subsetsl :: Int->[Int]->[[Int]]
subsetsl 0 _ = [ [] ]
subsetsl n xs = [ p:ys | p:ps <- tails xs, ys <- subsetsl (n-1) ps ]
{-
subsets [] = [[]]}
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)
-}
--subsets-End

--myMap
myMap :: (a->b)->[a]->[b]
myMap f = foldr (\x -> \xs -> f x : xs) []
{-
Method2:
myMap f = foldr (:) [] (map f )
-}
--myMap-End

--remdup
remdup :: [Char]->[Char]
remdup [] = []
remdup (x:xs) = x : remdup (filter (/=x) (x:xs))
--remdup-End

--elemOcc
elemOcc :: Char->[Char]->Int
elemOcc k = length . filter (==k)
--elemOcc-End

--occurrences
occurrences :: [Char] -> [(Char, Int)]
occurrences lt = zip [elemA | elemA <- remdup lt] [elemOcc elemA lt | elemA <- remdup lt]
{-
Method2:
--[ (elemA, elemOcc elemA lt) | elemA <- remdup lt ]
-}
--occurrences-End


main = do
	print "countNeg :: [Int] -> Int"
	print $ countNeg [1, -2, 3, -5] -- 2
	print "raise :: Int -> Int -> Int"
	print $ raise 2 4 -- 16
	print "pairs :: Int -> Int -> [Int]"
	print $ pairs [1,2,1] -- [(0,1),(1,2),(2,1),(1,0)]
	print $ pairs [1] -- [(0,1),(1,0)]
	print "pascal :: Int -> [Int]"
	print $ pascal 2 -- [1,1]
	print $ pascal 3 -- [1,2,1]
	print $ pascal 4 -- [1,3,3,1]
	print $ pascal 5 -- [1,4,6,4,1]
	print "q1f1a :: [Int] -> [Int]"
	print $ q1f1a [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] -- [9,12,15,18,21,24,27,30]
	print "q1f1b :: [Int] -> [Int]"
	print $ q1f1b [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] -- [9,12,15,18,21,24,27,30]
	print "compre :: [a] -> (a -> b) -> (a -> Bool) -> [b]"
	print $ compre [1,2,3,4,5,6,7,8] (2*) (<4) -- [2,4,6]
	print "subsets :: [Int] -> [[Int]]"
	print $ subsets [1] -- [[1],[]]
	print $ subsets [1,2] -- [[1,2],[1],[2],[]]
	print $ subsets [1,2,3] -- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
	print $ subsets [1,2,3,4] -- [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
	print "myMap :: (a -> b) -> [a] -> [b]"
	print $ myMap (*2) [] -- []
	print $ myMap (*2) [1,-2,3,-4] -- [2,-4,6,-8]
	print $ myMap (abs) [1,-2,3,-4] -- [1,2,3,4]
	print "remdup :: [Char] -> [Char]"
	print $ remdup [] -- ""
	print $ remdup ['a'] -- "a"
	print $ remdup ['a','b'] -- "ab"
	print $ remdup ['a','b','a','a','b','b','c'] -- "abc"
	print "elemOcc :: Char -> [Char] -> Int"
	print $ elemOcc 'a' ['a'] -- 1
	print $ elemOcc 'a' [] -- 0
	print $ elemOcc 'a' ['a','b','a','a','b','b','c'] -- 3
	print "occurrences :: [Char] -> [(Char,Int)]"
	print $ occurrences ['a','c','d','a','c'] -- [('a',2),('c',2),('d',1)]
	print $ occurrences ['a','b','a','a','b','b','c'] -- [('a',3),('b',3),('c',1)]
