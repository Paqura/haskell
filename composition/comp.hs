import Data.List
import Data.Semigroup

-- getReversedTail :: [a] -> [a]
-- getReversedTail = reverse . tail

-- getMinVal :: Ord a -> a
-- getMinVal = head . sort

data Color = Red
  | Yellow
  | Blue
  | Purple
  | Orange
  | Green
  | Brown deriving (Show, Eq)

-- instance Semigroup Color where
--   (<>) Red Blue = Purple
--   (<>) Blue Red = Purple
--   (<>) Yellow Blue = Green
--   (<>) Blue Yellow = Green
--   (<>) Yellow Red = Orange
--   (<>) Red Yellow = Orange
--   (<>) a b = if a == b
--               then a
--               else Brown

-- пример
-- orange = Red <> Yellow

--------------------

-- охранные выражение
howMuch :: Int -> String
howMuch n | n > 10 = "куча"
          | n > 0 = "средне"
          | otherwise = "в минусах"


instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Red Yellow = Orange
  (<>) Yellow Red = Orange
  (<>) a b
      | a == b = a
      | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
      | all (`elem` [Yellow, Blue, Green]) [a, b] = Green
      | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
      | otherwise = Brown