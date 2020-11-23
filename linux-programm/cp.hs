import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO()
main = do
  args <- getArgs
  let filePath = head args
  let outputFile = head $ tail args

  fileContent <- TIO.readFile filePath
  TIO.appendFile outputFile fileContent

  putStrLn "copied"