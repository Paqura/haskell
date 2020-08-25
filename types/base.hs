double :: Int -> String
double n = show (n * 2)

makeAddress :: String -> String -> (String, String)
makeAddress house flat = (house, flat)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
  then f n
  else n

-- Типовые переменные (а-ля дженерики)
someFn :: a -> b -> c -> (a, b, c)
someFn param1 param2 param3 = (param1, param2, param3)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap fn (x:xs) = fn x : myMap fn xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter fn (x:xs) = if fn x
  then x : myFilter fn xs
  else myFilter fn xs

myFoldl :: (a -> a) -> a -> [a] -> a
myFoldl _ init [] = init
myFoldl fn init (x:xs) = myFoldl fn newInit xs
 where newInit = fn x  
