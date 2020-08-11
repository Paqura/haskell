main :: IO()

getName name = "Hello " ++ name ++ "!\n"
getCity city = "from " ++ city

createMessage name city = getName name ++ getCity city

main = do
  putStrLn "Как вас зовут?"
  name <- getLine
  putStrLn "Ваш город?"
  city <- getLine
  putStrLn (createMessage name city)