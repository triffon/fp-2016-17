module Main where

oddSquares = [ x^2 | x <- [1..], odd x ]

isPrime _ = True

twins = [ (x, x + 2) | x <- [3..], isPrime x, isPrime (x + 2) ]

pairs = [ (x, y) | x <- [0..], y <- [0..x] ]

pythagoreanTriples = [ (x, y, z) |
                       z <- [1..],
                       x <- [1..z-1],
                       y <- [1..x-1],
                       x^2 + y^2 == z^2,
                       gcd x y == 1]
                     
powers n = 1 : map (*n) (powers n)

triplets = iterate (map (+3)) [3,2,1]
