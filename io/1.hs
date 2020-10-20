greeting :: String -> String
greeting name = "Hello, " ++ name ++ "!"

main :: IO()
main = do
  putStrLn "What is your name?"
  name <- getLine
  let statetment = greeting name
  putStrLn statetment