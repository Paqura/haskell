-- main :: IO ()
-- main = do
--   userInput <- getContents
--   mapM_ print (lines userInput)

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  userInput <- getContents
  let number = toInts userInput
  print (sum number)