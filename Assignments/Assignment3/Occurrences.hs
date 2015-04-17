import Data.List
--occurrences :: [Char] -> [(Char, Int)]
occurrences lt = zip [elemA | elemA <- remdup lt] [elemOcc elemA lt | elemA <- remdup lt]
--[ (elemA, elemOcc elemA lt) | elemA <- remdup lt ]

remdup :: [Char]->[Char]
remdup [] = []
remdup (x:xs) = x : remdup (filter (/=x) (x:xs))

elemOcc :: Char->[Char]->Int
elemOcc k = length . filter (==k)
