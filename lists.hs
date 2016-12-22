module Lists where

import Prelude hiding
  (head, tail, null, length, (++),
   reverse, (!!), elem, init, last,
   take, drop, maximum, minimum,
   sum, product, map, filter,
   foldl, scanr, scanl, zip, unzip, zipWith)

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

pythagoreanTriples :: Int -> Int -> [(Int,Int,Int)]
pythagoreanTriples from to =
  [(a,b,c) | a <- [from..to], b <- [a+1..to],
             c <- [b..to], a^2 + b^2 == c^2]

init :: [a] -> [a]
init []     = error "Празен списък"
init [_]    = []
init (x:xs) = x:init xs

last :: [a] -> a
{-
last []     = error "Празен списък"
last [x]    = x
last (x:xs) = last xs
-}
last = foldl1 (const id)

take :: Int -> [a] -> [a]
take 0 _      = []
take n (x:xs) = x:take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 l      = l
drop n (_:xs) = drop (n-1) xs

maximum :: Ord a => [a] -> a
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

minimum :: Ord a => [a] -> a
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

map f = foldr (\x -> (f x:)) []

{-
filter p [] = []
filter p (x:xs) = if p x then x:r else r
  where r = filter p xs
-}

filter p = foldr (\x -> if p x then (x:) else id) []

scanr :: (a -> b -> b) -> b -> [a] -> [b]
{-
scanr _ nv []      = [nv]
scanr op nv (x:xs) = x `op` r : s
  where s@(r:_) = scanr op nv xs
-}

scanr op nv = foldr (\x s@(r:_) -> x `op` r : s) [nv]

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _  nv []     = nv
foldl op nv (x:xs) = foldl op (nv `op` x) xs

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl op nv []     = [nv]
scanl op nv (x:xs) = nv : scanl op (nv `op` x) xs

-- scanl op nv = foldl (\r x -> r:(r `op` x)) [nv]

{-
zip :: [a] -> [b] -> [(a,b)]
zip [] _  = []
zip _  [] = []
zip (x:xs) (y:ys) = (x,y):zip xs ys
-}

unzip :: [(a,b)] -> ([a],[b])
unzip []          = ([],[])
unzip ((x,y):xys) = (x:xs,y:ys)
  where (xs,ys) = unzip xys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith op [] _  = []
zipWith op _  [] = []
zipWith op (x:xs) (y:ys) = (x `op` y):zipWith op xs ys

zip = zipWith (,)
