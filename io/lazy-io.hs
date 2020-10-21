import System.Environment
import Control.Monad

-- пример перебора IO значений
-- mapM - M - Monad, нижнее подчеркивание, что мы отбрасываем результаты
-- main :: IO ()
-- main = do
--   args <- getArgs
--   mapM_ putStrLn args

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let linesToRead
--         | not (null args) = read (head args)
--         | otherwise = 0 :: Int
--   print linesToRead

-- это что-то вроде калькулятора,
-- но для сложения онли
-- вводим кол-во чисел
-- и консоль ожидает их и потом складывает

main :: IO ()
main = do
  args <- getArgs
  let linesToRead
          | not (null args) = read (head args)
          | otherwise = 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)