import System.IO
import System.Environment

-- words & lines стандартные методы для подсчета, возвращают массив
getCounts :: String -> (Int, Int, Int)
getCounts input = (sCount, wCount, rCount)
  where
    sCount = length input
    wCount = length $ words input
    rCount = length $ lines input

-- функция визуализации
-- unwords что-то вроде ++ или mconcat, но он умеет так же работать с типом Text
countsText :: (Int, Int, Int) -> String
countsText (sc, wc, rc) = unwords [
      "symbols: "
    , show sc
    , " words: "
    , show wc
    , " rows: "
    , show rc
  ]

main :: IO()
main = do
  args <- getArgs
  let fileName = head args
  input <- readFile fileName
  let summary = (countsText . getCounts) input
  appendFile "stats.txt" $ mconcat [fileName, " ", summary, "\n"]
  putStrLn summary