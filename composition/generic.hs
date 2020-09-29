import Data.Maybe
import qualified Data.Map as Map

-- простой тип

data Box a = Box a deriving Show

wrap :: a -> Box a
wrap a = Box a

unwrap :: Box a -> a
unwrap (Box a) = a

-- Тип Triple
-- принимает три аргумента одинакового типа

data Triple a = Triple a a a deriving Show

type Point = Triple Double
aPoint :: Point

aPoint = Triple 0.6 0.5 0.89

first :: Triple a -> a
first (Triple x _ _ ) = x

second :: Triple a -> a
second (Triple _ x _) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform fn (Triple x y z) = Triple (fn x) (fn y) (fn z)

-- transform example
transformResult = transform (* 3) aPoint
-- Triple 1.7999999999999998 1.5 2.67

-- Список
-- по факту, это LinkedList

data List a = Empty | Cons a (List a) deriving Show

customMap :: (a -> b) -> List a -> List b
customMap _ Empty = Empty
customMap fn (Cons a rest) = Cons (fn a) (customMap fn rest)

-- example

customList :: List Int
customList = Cons 2 (Cons 3 (Cons 4 Empty))
customMapResult = customMap (* 2) customList

-- Типы можно давать нескольким переменным сразу
user, user2 :: String
user = "Slava"
user2 = "Vasya"

-- Кортежи
data Organ = Head | Hand | Pointer deriving (Show, Eq)

organs :: [Organ]
organs = [Head, Hand, Pointer, Pointer, Hand, Pointer]

getIdx :: [Organ] -> [Int]
getIdx [] = []
getIdx (x:xs) = result
  where currLen = length xs
        result | currLen == 0 = [0] ++ getIdx xs
               | otherwise = [currLen] ++ getIdx xs

organPairs = zip (reverse (getIdx organs)) organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

getOrganByIdx :: Int -> Maybe Organ
getOrganByIdx idx = Map.lookup idx organCatalog