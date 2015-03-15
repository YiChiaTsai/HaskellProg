import System.IO

main = do putStr "Please input your name:"
          hFlush stdout
	  name <- getLine
	  putStrLn("Hello! " ++ name ++ "!")
