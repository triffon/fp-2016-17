module Defs where

sumLastDigits n = lastDigit n +
                  lastDigit (stripDigit n)
  where lastDigit = (`mod` 10)
        stripDigit = (`div` 10)
        
fact 0 = 1
fact n = n * fact (n-1)

-- fact2 0 = 1
-- fact2 (n+1) = (n+1) * fact2 n

x #$% y = x * y + 8

{-
addVectors([X1,Y1],[X2,Y2],[X1+X2,Y1+Y2]).
-}

pow(x,0) = 1
pow(x,n) = x*pow(x,n-1)
