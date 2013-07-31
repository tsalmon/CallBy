{-
f n =
    [ a | a <- [
                if (null [y| y <- [2..(x-1)], x `mod` y == 0] )
                          then x else -1 | x <- [2..n] ] , a > 0]
-}

main = do 
  print (f 4)