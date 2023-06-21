factorial n | n < 1 = 1
            | otherwise = n * factorial (n - 1)

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq) 

height :: Tree a -> Integer
height Tip = 0
height (Bin left top right) = 1 + max (height left) (height right)
-- the height of a non-empty tree is 1 plus the height of the taller child

t = Bin (Bin Tip 1 Tip) 2 (Bin Tip 3 (bin Tip 4 Tip)

--leftChild (Bin left _ _) = left
--rightChild (Bin _ _ right) = right)

x :: Integer
x = 1

y :: Double 
y = 2.0 

z :: Rational
z = 1/3

w :: Complex Double
w = sqrt(-1)

-- Defining Map
map :: (a->b) -> [a] -> [a]
map f [] = []
map f (a:as) = f a : map f as

--zip With
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs

varience cs = sum (map sqdiff xs) / len
	where
	len = generic length xs
	mean = sum xs / len
	sqdiff x = (x - mean)^2
