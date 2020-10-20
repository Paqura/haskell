import Data.Map as Map

-- считаем диаметер
-- площать pi * r^2
-- диаметр pi * (r/2)^2
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2


type PizzaSize = Double
type PizzaCost = Double
type Pizza = (PizzaSize, PizzaCost)

-- стоимость сантиметра ^ 2
costPerCm :: Pizza -> Double
costPerCm (size, cost) = cost / diameter
  where diameter = areaGivenDiameter size

-- сравниваем и возвращаем дешевую
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas pz1 pz2
                  | costPz1 < costPz2 = pz1
                  | otherwise = pz2
  where
    costPz1 = costPerCm pz1
    costPz2 = costPerCm pz2

describePizza :: Pizza -> String
describePizza (size, cost) = "Пицца размера " ++
    show size ++ " дешевле по цене " ++
    show costSqCm ++ " за сантиметр ^ 2"
  where costSqCm = costPerCm (size, cost)

-- обычное решение

-- main :: IO ()
-- main = do
--   putStrLn "Enter size of first pizza"
--   size1 <- getLine
--   putStrLn "Enter cost of first pizza"
--   cost1 <- getLine
--   putStrLn "Enter size of second pizza"
--   size2 <- getLine
--   putStrLn "Enter cost of second pizza"
--   cost2 <- getLine
--   let pizza1 = (read size1, read cost1)
--   let pizza2 = (read size2, read cost2)
--   let betterPizza = comparePizzas pizza1 pizza2
--   putStrLn (describePizza betterPizza)

-- решение на монадах

costData :: Map.Map Int PizzaCost
costData = Map.fromList [(1, 150), (2, 220)]

sizeData :: Map.Map Int PizzaSize
sizeData = Map.fromList [(1, 30), (2, 50)]

maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
