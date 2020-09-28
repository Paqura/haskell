import Data.List
import Data.Semigroup

-- getReversedTail :: [a] -> [a]
-- getReversedTail = reverse . tail

-- getMinVal :: Ord a -> a
-- getMinVal = head . sort

data Color =
  Transparent
  | Red
  | Yellow
  | Blue
  | Purple
  | Orange
  | Green
  | Brown deriving (Show, Eq)

-- охранные выражение
howMuch :: Int -> String
howMuch n | n > 10 = "куча"
          | n > 0 = "средне"
          | otherwise = "в минусах"


instance Semigroup Color where
  (<>) Transparent any = any
  (<>) any Transparent = any
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

instance Monoid Color where
  mempty = Transparent
  mappend l1 l2 = l1 <> l2