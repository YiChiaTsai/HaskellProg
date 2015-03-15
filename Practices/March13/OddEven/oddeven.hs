import System.IO

main = do
	putStr "Please type a Integer:"
        hFlush stdout
        input <- getLine
        let number = read input::Int
            result = if number `mod` 2 == 0 then "Even" else "Odd"
        putStrLn(input ++ " is " ++ result ++ "!")
