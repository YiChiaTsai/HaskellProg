import System.IO

prompt :: String -> IO()
prompt text = do
		putStr text
		hFlush stdout

descOddEven :: Int -> String
descOddEven number = 
	if number `mod` 2 == 0 then "Even" else "Odd"

main = do
	prompt "Please input a integer:"
	input <- getLine
 	let desc = descOddEven(read input::Int)
 	putStrLn(input ++ " is " ++ desc ++ "!")
