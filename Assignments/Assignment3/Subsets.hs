import Data.List (tails)
subsets :: [Int] -> [[Int]]
subsets lt = foldr (++) [] [subsetsl elem lt | elem <- [0..(length lt)] ]
--subsets lt = concat [subsetsl elem lt | elem <- [0..(length lt)] ]

subsetsl :: Int->[Int]->[[Int]]
subsetsl 0 _ = [ [] ]
subsetsl n xs = [ p:ys | p:ps <- tails xs, ys <- subsetsl (n-1) ps ]
