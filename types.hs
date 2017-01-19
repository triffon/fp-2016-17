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

data PlayerN = PlayerN { name :: String,
                         score :: Int }
             deriving (Eq, Ord, Show, Read)

katnissN = PlayerN "Katniss Everdeen" 45
marioN = PlayerN { score = 48,
                   name  = "Mario" }

data Shape = Circle { radius :: Double} |
             Rectangle { width :: Double,
                         height :: Double }
           deriving (Eq, Ord, Show, Read)

circle :: Shape
circle = Circle 2.3

searchBest :: [PlayerN] -> Either Int [String]
searchBest players
 | (length bestPlayers) > 1 = Right $ map name bestPlayers
 | otherwise                = Left best
 where best = maximum $ map score players
       bestPlayers = filter ((==best). score) players

data Nat = Zero | Succ Nat
five = Succ $ Succ $ Succ $ Succ $ Succ $ Zero

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n

data Bin = One | BitZero Bin | BitOne Bin
  deriving (Eq, Ord, Show, Read)

six = BitZero $ BitOne $ One

fromBin :: Bin -> Int
fromBin One = 1
fromBin (BitZero n) = 2 * fromBin n
fromBin (BitOne n)  = 2 * fromBin n + 1

succBin :: Bin -> Bin
succBin One = BitZero One
succBin (BitZero n) = BitOne n
succBin (BitOne n)  = BitZero (succBin n)

data BinTree a = Empty |
                 Node { root :: a,
                        left :: BinTree a,
                        right :: BinTree a }
  deriving (Eq, Ord, Show, Read)

binLeaf :: a -> BinTree a
binLeaf x = Node x Empty Empty

t = Node 1 (Node 3 (binLeaf 2) (binLeaf 4))
           (Node 7 Empty (binLeaf 9))

depth :: BinTree a -> Int
depth Empty = 0
depth (Node _ l r) = max (depth l) (depth r) + 1

leaves :: BinTree a -> [a]
leaves Empty                = []
leaves (Node x Empty Empty) = [x]
leaves (Node _ l     r    ) = leaves l ++ leaves r

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree _ Empty = Empty
mapBinTree f (Node x l r) = Node (f x)
                            (mapBinTree f l)
                            (mapBinTree f r)

foldrBinTree :: (a -> a -> a) -> a -> BinTree a -> a
foldrBinTree _  nv Empty        = nv
foldrBinTree op nv (Node x l r) =
  (foldrBinTree op nv l) `op`
     (x `op`
       (foldrBinTree op nv r))

data Tree a = Tree { rootTree :: a,
                     subtrees :: TreeList a }
  deriving (Eq, Ord, Show, Read)

data TreeList a = None |
                  SubTree { firstTree :: Tree a,
                            restTrees :: TreeList a }
  deriving (Eq, Ord, Show, Read)

leaf x = Tree x None
tree = Tree 1 $ SubTree (leaf 2)
              $ SubTree (Tree 3
                              (SubTree (leaf 4) None))
              $ SubTree (leaf 5) None

level :: Int -> Tree a -> [a]
level 0 (Tree x _ ) = [x]
level k (Tree _ ts) = levelTrees k ts

levelTrees :: Int -> TreeList a -> [a]
levelTrees _ None           = []
levelTrees k (SubTree t ts) =
  level (k-1) t ++ levelTrees k ts

data SExpr = SInt Int | SChar Char |
             SList { getList :: [SExpr] }
             deriving (Eq, Ord, Show, Read)

se = SList [SInt 2, SChar 'a',
            SList [SChar 'b', SInt 5, SList []]]

countAtoms :: SExpr -> Int
countAtoms (SList ses) = sum $ map countAtoms ses
countAtoms _           = 1

flatten :: SExpr -> SExpr
flatten (SList ses) = SList $ concat $
                      map (getList . flatten) ses
flatten atom        = SList [atom]
