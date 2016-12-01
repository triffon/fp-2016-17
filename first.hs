module First where

{-
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)
-}

x :: Int
x = 2

y :: Double
y = fromIntegral x^2 + 7.5

z :: String
z = "Hello, world!"
-- !!! z = x + y

square :: Int -> Int
square x = x * x

div50 :: Int -> Int
div50 = let x = 50 in div x

twice f x = f (f x)

diag f x = f x x

mod13 = mod 13

lastDigit = (`mod` 10)

fact :: Integer -> Integer
fact n
 | n == 0  = 1
 | n > 0   = n * fact (n - 1)
 | n < 0   = error "Отрицателен аргумент"
