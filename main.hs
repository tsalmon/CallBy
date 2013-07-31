import System.Environment
import System.IO

fctn_list = []



-- insert txt into str at position p
insert str txt p = 
  if(p < (length str)) then 
    let a = (take p . drop 0) str in
    let b = (take (length str) . drop p) str in
    a ++ txt ++ b
  else
    error " > str"

algo x = print x

readfile f = do
  outh <- openFile f ReadMode
  x <- hGetContents outh
  hClose outh

main = do
  x <- getArgs
  readfile (head x)
  