import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

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
    -- (`Map.lookup` timeValueMap) == (\v -> Map.lookup v timeValueMap)
    extendedValues = map (`Map.lookup` timeValueMap) completeTimes

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

-- k v - key value
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
    updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
    -- тут опять махинация с лямбдой
    combinedValues = map (`Map.lookup` updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

-- ts1 <> ts2
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
-- 11|201.6
-- 12|201.5
-- 13|201.5
-- 14|203.5
-- 15|204.9
-- 16|207.1
-- 17|NA
-- 18|210.5
-- 19|NA
-- 20|208.8

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

-- mconcat [ts1, ts2]
-- получим тот же результат что и ts1 <> ts2

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

-- mean - среднее арифмитическое
mean :: (Real a) => [a] -> Double
mean xs = total / count
  where
    total = (realToFrac . sum) xs
    count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS _ values)
  | all (== Nothing) values = Nothing
  | otherwise = Just avg

  where
    justVals  = filter isJust values
    cleanVals = map fromJust justVals
    avg       = mean cleanVals

-- Функция сравнения

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where
    newFunc (i1, Nothing) (_, Nothing) = (i1, Nothing)
    newFunc (_, Nothing) (i, val) = (i, val)
    newFunc (i, val) (_, Nothing) = (i, val)
    newFunc (i1, Just val1) (i2, Just val2)
      | func val1 val2 == val1 = (i1, Just val1)
      | otherwise = (i2, Just val2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] []) = Nothing
compareTS func (TS times values)
  | all (== Nothing) values = Nothing
  | otherwise = Just best
  where
    pairs = zip times values
    best = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

-- Преобразование временных рядов

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues)
  where
    shiftValues = tail values
    diffValues  = zipWith diffPair shiftValues values

-- Скользящее среднее (average - avg - среднее)

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals
            | Nothing `elem` vals = Nothing
            | otherwise = Just avg
  where avg = mean (map fromJust vals)

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] _ = []
movingAvg vals n
              | length nextVals == n = meanMaybe nextVals : movingAvg restVals n
              | otherwise = []
  where
    nextVals = take n vals
    restVals = tail vals

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) _ = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
  where
    ma = movingAvg values n
    nothings = replicate (n `div` 2) Nothing
    smoothedValues = mconcat [nothings, ma, nothings]
