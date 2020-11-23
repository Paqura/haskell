{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- T.pack - конвертит String -> Text

getCounts :: T.Text -> (Int, Int, Int)
getCounts input = (sCount, wCount, rCount)
  where
    sCount = T.length input
    wCount = length $ T.words input
    rCount = length $ T.lines input

countsText :: (Int, Int, Int) -> T.Text
countsText (sc, wc, rc) = T.pack $ unwords [
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
  input <- TIO.readFile fileName
  let summary = (countsText . getCounts) input
  TIO.appendFile "stats.txt" $ mconcat [(T.pack fileName), " ", summary, "\n"]
  TIO.putStrLn summary