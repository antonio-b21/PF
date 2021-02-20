import Data.List

data Tree23
  = Empty
  | Node2 Char Tree23 Tree23
  | Node3 Char Char Tree23 Tree23 Tree23
  deriving (Show)

myTree :: Tree23
myTree = Node3 'a' 'b' (Node3 'b' 'a' (Node3 'c' 'c' Empty Empty Empty) (Node3 'd' 'a' Empty Empty Empty) (Node2 'a' Empty Empty)) (Node2 'a' (Node2 'b' Empty Empty) (Node2 'b' Empty Empty)) (Node3 'c' 'a' (Node2 'c' Empty Empty) (Node2 'd' Empty Empty) (Node2 'c' Empty Empty))

bma21treeToList :: Tree23 -> [Char]
bma21treeToList Empty = ""
bma21treeToList (Node2 c1 t1 t2) = c1 : bma21treeToList t1 ++ bma21treeToList t2
bma21treeToList (Node3 c1 c2 t1 t2 t3) =
  c1 :
  c2 :
  bma21treeToList t1 ++ bma21treeToList t2 ++ bma21treeToList t3

bma21exbAux :: String -> String -> [Char]
bma21exbAux arb cuv = foldr go "" arb
  where
    go :: Char -> String -> [Char]
    go c b
      | c `elem` cuv = nub (c : b)
      | otherwise = b

bma21exb :: Tree23 -> String -> [Char]
bma21exb arb = bma21exbAux (bma21treeToList arb)

testb1 = bma21exb myTree "ana are mere"

testb2 =  bma21exb myTree "ana are mere dulci"

testb3 =  bma21exb myTree "mere"

instance Eq Tree23 where
  (==) (Node3 ca cb ta tb tc) (Node3 cx cy tx ty tz) = ta == tx && tb == ty && tc == tz
  (==) (Node2 ca ta tb) (Node2 cx tx ty) =
      length (nub (ca : bma21treeToList ta ++ bma21treeToList tb)) == length (nub (cx : bma21treeToList tx ++ bma21treeToList ty))
      && bma21exbAux (ca : bma21treeToList ta ++ bma21treeToList tb) (cx : bma21treeToList tx ++ bma21treeToList ty)
      == bma21exbAux (cx : bma21treeToList tx ++ bma21treeToList ty) (ca : bma21treeToList ta ++ bma21treeToList tb)
  (==) _ _ = False

testc1 =  Node2 'b' Empty Empty == Node2 'b' (Node2 'c' Empty Empty) Empty

testc2 =  Node2 'b' Empty (Node2 'c' Empty Empty) == Node2 'b' (Node2 'c' Empty Empty) Empty

data Tree2
  = Vid
  | N Char Tree2 Tree2
  deriving Show

bma21exd :: Tree23 -> Tree2
bma21exd Empty = Vid
bma21exd (Node2 c t1 t2) = N c (bma21exd t1) (bma21exd t2)
bma21exd (Node3 c1 c2 t1 t2 t3) = N c1 (bma21exd t1) (N c2 (bma21exd t2) (bma21exd t3))

testd1 = bma21exd (Node2 'F' (Node2 'a' Empty Empty) (Node3 'd' 'e' Empty Empty Empty))