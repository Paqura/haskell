myLength [] = 0
{- получается, что 1 прибавляется к 1 пока массив не станет пустым и брать хвост будет не у чего  -}
myLength (x:xs) = 1 + myLength xs

{- сначала смотрим корнер кейсы, когда нужно взять 0 эл-ов или массив пустой -}
myTake 0 _ = []
myTake _ [] = []

{- через банальную рекурсию решаем задачу -}
myTake n (x:xs)  = x:rest
  where rest = myTake (n - 1) xs

{- просто конкатенируем первое значение с остатков (массивом) слепленным с первым значением бесконечно -}
myCycle (x:xs) = x : myCycle (xs++[x])

{- корнер кейсы: если эл-ов 0 возвращаем массив, если пустой массив возвращаем пустой массив -}
myDrop 0 aList  = aList
myDrop _ [] = []

myDrop n (x:xs) = myDrop (n - 1) xs
   
