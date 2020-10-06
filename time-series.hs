-- import Data.List
import qualified Data.Map as Map
-- import Data.Semigroup
-- import Data.Maybe

type File = [(Int, Double)]

file1 :: File
file1 = [(1, 200.1), (2, 199.5), (3, 199.4), (4, 198.9)
        , (5, 199.0), (6, 200.2), (9, 200.3), (10, 201.2)
        , (12, 202.9)]

file2 :: File
file2 = [(11, 201.6), (12, 201.5), (13, 201.5), (14, 203.5)
        , (15, 204.9), (16, 207.1), (18, 210.5), (20, 208.8)]

file3 :: File
file3 = [(10, 201.6), (11, 201.5), (12, 201.5), (13, 203.5)
        , (14, 204.9), (17, 207.1), (24, 210.5), (25, 208.8)]

file4 :: File
file4 = [(26, 201.6), (27, 201.5), (28, 201.5), (29, 203.5)
        , (30, 204.9), (31, 207.1), (32, 210.5), (32, 208.8)
        , (33, 220.1), (34, 219.8), (35, 220.5)]

-- Time Series
data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where
    -- здесь сортируются данные
    -- если список будет [1, 5, 2, 3] то возьмётся минимальное значение,
    -- потом максимальное и создастся массив от мин до мах [1..5]
    completeTimes = [minimum times .. maximum times]
    timeValueMap = Map.fromList (zip times values)
    {- HLINT -}
    extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where (times, values) = unzip tvPairs

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

-- ts1
-- 1|200.1
-- 2|199.5
-- 3|199.4
-- 4|198.9
-- 5|199.0
-- 6|200.2
-- 7|NA
-- 8|NA
-- 9|200.3
-- 10|201.2
-- 11|NA
-- 12|202.9
