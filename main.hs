import System.Environment
import System.IO

readfile f = do
  outh <- openFile f ReadMode
  x <- hGetContents outh
  putStrLn x
  hClose outh

main = do
  x <- getArgs
  readfile (head x)
  