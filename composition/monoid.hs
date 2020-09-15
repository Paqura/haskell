-- реализация орла и решки

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createTable :: Events -> Probs -> PTable
createTable events probs = PTable events normalizedProbs
  where totalProbs     = sum probs
        normalizedProbs = map (\x -> x/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

-- zipWith функция которая складывает два списка в зависимисто от аргумента
-- zipWith (+) [1, 2, 3] [4, 5, 6]  - [5, 7, 9]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs


