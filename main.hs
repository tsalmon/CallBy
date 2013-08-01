import System.Environment
import System.IO
import Text.Regex.Posix
import Text.Regex(subRegex,mkRegex)
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
  
-- add e to last l
addlastlist l e = (init l)++[(last l)++[e]]

-- insert txt into str at position p 
insert str txt p = 
  if(p < (length str)) then 
    let a = (take p . drop 0) str in
    let b = (take (length str) . drop p) str in
    a ++ txt ++ b
  else
    error "out of length"
      
isCsep x = x == ' ' || x== ';' || x == '\n' || x == ',' || x == '('

algo_read x = 
  let aux pos fun acc = 
        if(pos < (length x)) then
          let getname buf_str buf_pos = 
                if(isCsep(x!!buf_pos)) then "1" else "2"
          in case (x!!pos) of
            '(' -> if (acc > 0) then aux (pos+1) (fun++([last fun])) (acc) 
                   else aux (pos+1) (fun++[[getname "" 0]]) (acc)
            '{' -> aux (pos+1) fun (acc+1)
            '}' -> aux (pos+1) fun (acc-1)
            _   -> aux (pos+1) fun acc
        else
          show fun
  in aux 1 [] 0

readfile f = do
  outh <- openFile f ReadMode
  x <- hGetContents outh
  putStrLn (algo_read x)
  hClose outh

main = do  
  x <- getArgs
  readfile (head x)
  