gettriangle = [[a, b, c] | a <- [1 .. 10], b <- [1 .. 10], c <- [1 .. 10], a <= b, b <= c, a^2 + b^2 == c^2]
