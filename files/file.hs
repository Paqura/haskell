import System.IO

-- аннотация типов из System.IO
-- type FilePath = String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- openFile :: FilePath -> IOMode -> IO Handle

-- main :: IO()
-- main = do
--   myFile <- openFile "text.txt" ReadMode
--   hClose myFile
--   putStrLn "done!"

-- меняем местами строки
-- main :: IO()
-- main = do
--   textFile <- openFile "text.txt" ReadMode
--   firstLine <- hGetLine textFile
--   secondLine <- hGetLine textFile
--   outputFile <- openFile "output.txt" WriteMode
--   hPutStrLn outputFile secondLine
--   hPutStrLn outputFile firstLine
--   hClose outputFile
--   putStrLn "done!"

-- проверка на существование
-- main :: IO()
-- main = do
--   textFile <- openFile "text.txt" ReadMode
--   hasLine <- hIsEOF textFile
--   firstLine <- if not hasLine
--                 then hGetLine textFile
--               else return "empty file"
--   putStrLn firstLine
--   putStrLn "done!"

