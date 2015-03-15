palindrome :: [Int] -> Bool
palindrome [] = True
palindrome [_] = True
palindrome lt = (head lt) == (last lt) && (palindrome $ init $ tail lt)
