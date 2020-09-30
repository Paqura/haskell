import qualified Data.Map as Map
import Data.Maybe

data Organ = Heart | Brain | Kidney | Spleen deriving Eq

instance Show Organ where
  show Heart = "Сердце"
  show Brain = "Мозг"
  show Kidney = "Почка"
  show Spleen = "Селезенка"

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- drawers - ящик
possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawersContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawersContents ids catalog = map getContents ids
  -- модно молодежно
  -- \id -> Map.lookup id catalog ==  (`Map.lookup` catalog)
  where getContents = (`Map.lookup` catalog)

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawersContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available  = length (filter isMatch available)
  where isMatch = \x -> x == Just organ

-- в импортированном Data.Map
-- есть методы isJust, isNothing

justTheOrgans = filter isJust availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan Nothing = ""
showOrgan (Just organ) = show organ

organList :: [String]
organList = map showOrgan justTheOrgans

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " в ванночке"
  show (Cooler organ) = show organ ++ " в холодосе"
  show (Bag organ) = show organ ++ " в сумке"

data Location = Lab | Kithen | Bathroom

instance Show Location where
  show Lab = "лаборатория"
  show Kithen = "кухня"
  show Bathroom = "душевая"

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kithen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) =
  show container ++
  " (место: " ++
  show location ++ ")"

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "Ошибка. Идентификатора нет"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where organ = Map.lookup id catalog

-- пример
positive = processRequest 13 organCatalog
-- "Мозги в ванночке (место: Лаборатория)"
negative = processRequest 12 organCatalog
-- "Ошибка. Идентификатора нет"

----------------

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap fn Nothing = Nothing
maybeMap fn (Just x) = Just (fn x)