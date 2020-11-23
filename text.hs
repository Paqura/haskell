{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- main :: IO()
-- main = do
--   print $ T.splitOn " " "Work with with"
-- ["Work", "with", "with"]

-- что-то типо `join`
-- main :: IO()
-- main = do
--   print $ T.intercalate " " ["Work", "with", "with"]

-- combinedTextMonoid :: T.Text
-- combinedTextMonoid = mconcat ["some", " ", "text"]

-- combinedTextSemigroup :: T.Text
-- combinedTextSemigroup = "some" <> " " <> "text"

-- word with io
-- helloPerson :: T.Text -> T.Text
-- helloPerson name = T.intercalate " " ["Hello", name]

-- main :: IO()
-- main = do
--   TIO.putStrLn "Hello! What are you name?"
--   name <- TIO.getLine
--   let statement = helloPerson name
--   TIO.putStrLn statement

-- text hightlighter
dharma :: T.Text
dharma = "hell"

bgText :: T.Text
bgText = "hell green color brahell hell few times"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where
    pieces = T.splitOn query fullText
    highlighted = mconcat ["{", query, "}"]

main :: IO()
main = do
  TIO.putStrLn $ highlight dharma bgText
