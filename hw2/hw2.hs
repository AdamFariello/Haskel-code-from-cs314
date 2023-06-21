-- Context
data Poly a = P [a] deriving (Show, Eq)

-- Question 1
degree :: Poly a -> Int
degree (P []) = 0
degree (P (_:[])) = 0
degree (P (_:xs)) = 1 + degree (P xs) 

-- Question 2
scale :: (Num a, Eq a) => a -> Poly a -> Poly a
scale 0 (P _) = P []
scale n (P x) = P (map (*n) x)

-- Question 3
($$) :: (Num a, Eq a) => Poly a -> a -> a
(P []) $$ _ = 0
(P (x:[])) $$ _ = x
(P x) $$ n = ((n^degree(P x)) * last(x)) + ((P (init(x))) $$ n)

-- Question 4
-- Helper Function stolen from Data.list
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

addPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
addPoly (P x) (P y)  
    | (length x < length y) = P (dropWhileEnd (==0) (zipWith (+) y (x ++ (replicate(length y - length x) 0))))
    | otherwise             = P (dropWhileEnd (==0) (zipWith (+) x (y ++ (replicate(length x - length y) 0))))

-- Question 5
--n := degree that will be used to push it forward
scalePoly n x   = (replicate n 0) ++ x

multPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
multPoly (P []) (P x) = P [] 
--multPoly (P y) (P x)  = P (scale (last y) (P (scalePoly (degree (P y)) x)))
--multPoly (P y) (P x)  = P (map (* last y) (scalePoly (degree (P y)) x)) 
{-
multPoly (P y) (P x)  = P (
    addPoly 
      (P (map (* last y) (scalePoly (degree (P y)) x))) 
      (P (multPoly (P (init y)) (P x)))
) 
-}
multPoly (P y) (P x)  = P (addPoly (P (map (* last y) (scalePoly (degree (P y)) x))) (P (multPoly (P (init y)) (P x)))) 
