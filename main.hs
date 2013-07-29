{-
	CALLBY
-}

isIn l k = case l of 
	[] -> False
	a:b -> a==k || isIn b k
