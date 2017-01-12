class Measurable a where
   size :: a -> Int
   empty :: a -> Bool
   empty x = size x == 0

instance Measurable Int where
  size 0 = 0
  size n = size (n `quot` 10) + 1

instance (Measurable a, Measurable b) => Measurable (a,b) where
  size (x,y) = size x + size y

instance Measurable a => Measurable [a] where
  size = sum . map size
