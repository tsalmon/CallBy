import System.Environment
import System.IO  
{- 
1)read the file given in parameters
2)str is function declaration? -> add his name in fctn_list, enter in it [[name]...]
3)for all call of functions, add their names add after the name of the function [[name, c1, .., cn]]
4)at the end of file, read again the file
5)if str is a function declaration:
  call = "" 
  for all elems in fctn_list as elem:
     for all elems in elem[1..]: 
        if elems == str then
           call += elem[0]
  insert file_text call (position str -1)
6)write file_text in the file
-}
  
fctn_list = []
            
-- insert txt into str at position p 
insert str txt p = 
  if(p < (length str)) then 
    let a = (take p . drop 0) str in
    let b = (take (length str) . drop p) str in
    a ++ txt ++ b
  else
    error "out of length"
  
algo x = print x

readfile f = do
  outh <- openFile f ReadMode
  x <- hGetContents outh
  hClose outh

main = do
  x <- getArgs
  readfile (head x)
  