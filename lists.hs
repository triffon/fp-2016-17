module Lists where

import Prelude hiding
  (head, tail, null, length, (++),
   reverse, (!!), elem)

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
-- tail (_:xs) = xs
tail l = case l of (_:xs) -> xs

null :: [a] -> Bool
-- null [] = True
-- null _  = False

null l = case l of [] -> True
                   _  -> False

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ b = b
(x:xs) ++ b = x:xs ++ b

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(!!) :: [a] -> Int -> a
[]     !! _ = error "Невалиден индекс"
(x:_ ) !! 0 = x
(_:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ []     = False
-- !!!! elem x (x:xs) = True
elem x (y:ys) = x == y || elem x ys
{- | x == y     = True
 | otherwise  = elem x ys
-}

