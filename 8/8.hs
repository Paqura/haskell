{- HLINT ignore -}
myMap fn [] = []
myMap fn (x:xs) = fn x : myMap fn xs

myFilter fn [] = []
myFilter fn (x:xs) =
  if fn x
    then x : myFilter fn xs
  else myFilter fn xs

myRemove fn [] = []
myRemove fn (x:xs) =
  if fn x
    then myRemove fn xs
  else x : myRemove fn xs

-- разворот
rcons xs y = y:xs
myReverse xs = foldl rcons [] xs

-- тут initVal уже будет финальным результатом
myFoldl operand initVal [] = initVal
myFoldl operand initVal (x:xs) = myFoldl operand result xs
  where result = initVal (operand) x
