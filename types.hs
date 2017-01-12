--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE FlexibleInstances #-}
module Types where

class Measurable a where
  size :: a -> Int
  empty :: a -> Bool
  empty x = size x == 0


instance Measurable Int where
  size 0 = 0
  size n = size (n `quot` 10) + 1


{-
instance (Integral a) => Measurable a where
  size 0 = 0
  size n = size (n `quot` 10) + 1
-}
  
instance (Measurable a, Measurable b) =>
  Measurable (a, b) where
  size (x,y) = size x + size y

instance (Measurable a) => Measurable [a] where
  size = sum . map size

type Matrix a = [[a]]
-- typedef

data Player = Player Name Score
type Name = String
type Score = Int

katniss = Player "Katniss Everdeen" 45
mario = Player "Mario" 48

better :: Player -> Player -> String
better (Player name1 score1) (Player name2 score2)
 | score1 > score2  = name1
 | otherwise        = name2

-- data (,) a b = (,) a b

type Player2 = (Name, Score)

katniss2 = ("Katniss Everdeen",45)
mario2   = ("Mario", 48)

better2 (name1, score1) (name2, score2)
 | score1 > score2  = name1
 | otherwise        = name2
