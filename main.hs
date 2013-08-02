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
  

--checkMain motif lang = (lang == "c" && motif == "main") || (lang == "java" && motif == "main")

-- insert txt into str at position p 
insert str txt p = 
  if(p < (length str)) then 
    let a = (take p . drop 0) str in
    let b = (take (length str) . drop p) str in
    a ++ txt ++ b
  else
    error "out of length"
      
isCsep x = x == ' ' || x== ';' || x == '\n' || x == ',' || x == '('

--rm duplicate elem
no_double [] = []
no_double (e:l)= e:(no_double (filter (/=e) l))
browse_list [] = []
browse_list (e:l) = (no_double e):(browse_list l) 

-- add e to last l
addlastlist l e = (init l)++[(last l)++[e]]

-- read file x
algo_read x = 
  let aux pos fun acc = 
        if(pos < (length x)) then
          let getname buf_str buf_pos = 
                if(buf_pos > 0 && (x!!buf_pos) /= ' ') 
                then getname ((x!!buf_pos):buf_str) (buf_pos-1) 
                else buf_str
          in case (x!!pos) of
              '(' -> if (acc > 0) then aux (pos+1) (addlastlist fun (getname "" (pos-1))) (acc) 
                     else aux (pos+1) (fun++[[getname "" (pos-1)]]) (acc)
              '{' -> aux (pos+1) fun (acc+1)
              '}' -> aux (pos+1) fun (acc-1)
              _   -> aux (pos+1) fun acc
        else
          algo_write x (browse_list fun)
  in aux 1 [] 0

-- for a method (str), get calles methodes
call_by str [] = ""
call_by str (e:l) = if (str `elem` (tail e)) then (head e) ++ ", "++ (call_by str l) else (call_by str l)  

algo_write str l = 
  let aux pos new_str acc = 
        if (pos < (length new_str)) then
          let writePos buf_pos  = 
                if ((new_str!!buf_pos) == '\n' || (new_str!!buf_pos) == ';' || (new_str!!buf_pos) == '}') 
                then buf_pos  
                else (writePos (buf_pos - 1)) in
          let getname buf_str buf_pos = 
                if(buf_pos > 0 && (new_str!!buf_pos) /= ' ') 
                then getname ((new_str!!buf_pos):buf_str) (buf_pos-1) 
                else buf_str
          in case (new_str!!pos) of
            '(' -> if (acc == 0) then 
                     let add_txt = "\n call by :" ++ (call_by (getname "" (pos-1)) l) ++ "\n" in
                     aux (pos+(length add_txt)+1) (insert new_str (add_txt) (writePos (pos-1))) (acc) 
                   else aux (pos+1) new_str (acc)
            '{' -> aux (pos+1) new_str (acc+1)
            '}' -> aux (pos+1) new_str (acc-1)
            _   -> aux (pos+1) new_str acc
        else
          putStrLn (new_str)
  in aux 0 str 0

readfile f = do
  outh <- openFile f ReadMode
  x <- hGetContents outh
  (algo_read x) --TODO: create method checkLanguage
  hClose outh

main = do 
  x <- getArgs
  readfile (x!!0)