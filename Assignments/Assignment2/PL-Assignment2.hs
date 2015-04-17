--power1
power1 :: Int -> Int -> Int
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product $ replicate k n
--power1-End

--power2
power2 :: Int -> Int -> Int
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k =
	if even k
	then power2 (n^2) (div k 2)
	else n * power2 n (k-1)
--power2-End


--myButLast
myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast(x:xs) = myButLast xs
{-
Method2:
myButLast :: [a] -> a
myButLast lt = do
	let (x:xs) = reverse lt
	head xs
-}
{-
Method3:
myButLast :: [a] -> a
myButLast lt = last $ init lt
-}
--myButLast-End


--rev2
rev2 :: [a] -> [a]
rev2 [x, y] = y:[x]
rev2 (x:xs) = x:xs
{-
--Method2:
rev2 :: [a] -> [a]
rev2 lt =
	if leng lt == 2
	then reverse lt
	else lt

leng :: [a] -> Int
leng lt =
	if null lt
	then 0
	else 1 + (leng $ tail lt)
-}
--rev2-End


--tailUpto with tail-recursive
tailUpto :: Int->Int->[Int]->[Int]
tailUpto m n lt
	| m>n = lt
	| otherwise = tailUpto m (n-1) (n:lt)

upto :: Int->Int->[Int]
upto m n = tailUpto m n []
{-
Method 2:
tailUpto m n lt = [m..n] ++ lt
-}
--tailUpto-End


--tailFib with tail-recursive
tailFib :: Int->Int->Int->Int
tailFib 0 0 1 = 0
tailFib 1 0 1 = 1
tailFib n acc1 acc2
	| n==2 = acc1+acc2
	| n>2 = tailFib (n-1) acc2 (acc1+acc2)

fib :: Int -> Int
fib n = tailFib n 0 1
--tailFib-End


--palindrome
palindrome :: [Int] -> Bool
palindrome [] = True
palindrome [_] = True
palindrome lt = (head lt) == (last lt) && (palindrome $ init $ tail lt)
--palindrome-End


--isPermutation
removeOnce :: Int->[Int]->[Int]
removeOnce x [] = []
removeOnce n (x:xs) | x==n = xs
			   	    | otherwise = x: removeOnce n xs
{-
Method2:
removeOnce x lt =
	if head lt /= x
	then head lt : removeOnce x (tail lt)
	else tail lt
-}

isPermutation :: [Int] -> [Int] -> Bool
isPermutation [] [] = True
isPermutation [_] [] = False
isPermutation [] [_] = False
isPermutation lt1 lt2 = do
	let lt3 = removeOnce (head lt1) lt2
	if lt3 == lt2
	then False
	else isPermutation (tail lt1) lt3
--isPermutation-End


main = do
	print $ "Following is testing from github!"
	print $ "power1 :: Int -> Int -> Int"
	print $ power1 1 100 -- 1
	print $ power1 2 1 -- 2
	print $ power1 2 10 -- 1024
	print $ power1 2 0 -- 1
	print $ "power2 :: Int -> Int -> Int"
	print $ power2 1 100 -- 1
	print $ power2 2 1 -- 2
	print $ power2 2 10 -- 1024
	print $ power2 2 0 -- 1
	print $ "myButLast :: [a] -> a"
	print $ myButLast [1,2,3,4] -- 3
	print $ myButLast [5,6] -- 5
	print $ myButLast ['a','b','c','d'] -- 'c'
	print $ myButLast ['e','f'] -- 'e'
	print $ myButLast ['a'..'z'] -- 'y'
	print $ "rev2 :: [a] -> [a]"
--	print $ rev2 []
	print $ rev2 [1] -- [1]
	print $ rev2 [1,2] -- [2,1]
	print $ rev2 [1,2,3] -- [1,2,3]
	print $ rev2 [1,2,3,4] -- [1,2,3,4]
	print $ rev2 ['a'] -- "a"
	print $ rev2 ['a','b'] -- "ba"
	print $ rev2 ['a','b','c'] -- "abc"
	print $ rev2 ['a','b','c','d'] -- "abcd"
	print $ "tailUpto :: Int -> Int -> [Int] -> [Int]"
	print $ tailUpto 3 8 [1,2,3] -- [3,4,5,6,7,8,1,2,3]
	print $ tailUpto 8 3 [1] -- [1]
	print $ tailUpto (-3) 8 [1,2,3] -- [-3,-2,-1,0,1,2,3,4,5,6,7,8,1,2,3]
	print $ tailUpto 0 0 [1,2,3] -- [0,1,2,3]
	print $ tailUpto 0 0 [] -- [0]
	print $ "tailFib :: Int -> Int -> Int -> Int"
	print $ tailFib 0 0 1 -- 0
	print $ tailFib 1 0 1 -- 1
	print $ tailFib 2 0 1 -- 1
	print $ tailFib 3 0 1 -- 2
	print $ tailFib 4 0 1 -- 3
	print $ tailFib 5 0 1 -- 5
	print $ tailFib 6 0 1 -- 8
	print $ "palindrome :: [Int] -> Bool"
	print $ palindrome [1,2,3,3,2,1] -- True
	print $ palindrome [1,2,3,4,2,1] -- False
	print $ palindrome [1,2,4,2,1] -- True
	print $ palindrome [1,2,4,5,1] -- False
	print $ palindrome [1] -- True
	print $ palindrome [] -- True
	print "isPermutation :: [Int] -> [Int] -> Bool"
	print $ isPermutation [] [] -- True
	print $ isPermutation [1,2,1] [2,1,1] -- True
	print $ isPermutation [1,2,1] [2,1,2] -- False
	print $ isPermutation [1,1,1] [1] -- False
	print $ isPermutation [1,2,1] [1,2,1,1] -- False
	print $ isPermutation [1,2,1,1] [1,2,1] -- False
	print "removeOnce :: Int -> [Int] -> [Int]"
	print $ removeOnce 3 [1,2,3,4,5] -- [1,2,4,5]
	print $ removeOnce 6 [1,2,3,4,5] -- [1,2,3,4,5]
	print $ removeOnce 3 [1,2,3,7,3,3,3] -- [1,2,7,3,3,3]
	print $ removeOnce 3 [] -- []
