-- safe version of heads
heads :: [a] -> Maybe a
heads [] = Nothing
heads (x:xs) = Just x

-- safe version of last
final :: [a] -> Maybe a
final [] = Nothing
final (x:[]) = Just x
final (_:xs) = final xs

-- safe version of sumTree
data Tree a = Tip | Bin (Tree a) a (Tree a)
sumTree :: Num a => Tree a -> a
sumTree Tip = 0
sumTree (Bin Tip c Tip) = c
sumTree (Bin left c right) = sumTree(left) + c + sumTree(right)
-- or: sumTree (Bin l c r) = sumTree(l) + c + sumTree(r)
