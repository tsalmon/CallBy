{- 
2  []
3  []
4  [2]
5  []
6  [2, 3]
7  []
8  [2, 4]
9  [3]
10 [2, 5]

-}

f x = [ b |  a <- [1..10], b <- [2..(a-1)], a `mod` b / 0]

main = do 
  print (f 4)