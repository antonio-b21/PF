import qualified Data.List as List
import Prelude hiding (lookup)

class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert :: Ord key => key -> value -> c key value -> c key value
  lookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  toList :: c key value -> [(key, value)]

  keys :: c key value -> [key]
  keys c = [key | (key, _) <- toList c]

  values :: c key value -> [value]
  values c = [value | (_, value) <- toList c]

  fromList :: Ord key => [(key, value)] -> c key value
  fromList [] = empty
  fromList ((k, v) : list) = insert k v (fromList list)

--ex2
newtype PairList k v = PairList {getPairList :: [(k, v)]}
  deriving (Show)

instance Collection PairList where
  empty = PairList []
  singleton k v = PairList [(k, v)]
  insert k v (PairList list) = PairList ((k, v) : [(key, value) | (key, value) <- list, key /= k])
  lookup k (PairList []) = Nothing
  lookup k (PairList ((key, value) : list))
    | k == key = Just value
    | otherwise = lookup k (PairList list)
  delete k (PairList list) = PairList [(key, value) | (key, value) <- list, key /= k]
  toList (PairList list) = list

--ex3
data SearchTree key value
  = Empty
  | Node
      (SearchTree key value) -- elemente cu cheia mai mica
      key -- cheia elementului
      (Maybe value) -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare
  deriving (Show)

instance Collection SearchTree where
  empty = Empty
  singleton k v = Node Empty k (Just v) Empty
  insert k v Empty = singleton k v
  insert k v (Node arb1 key value arb2)
    | k < key = Node (insert k v arb1) key value arb2
    | k > key = Node arb1 key value (insert k v arb2)
    | otherwise = Node arb1 k (Just v) arb2
  lookup k Empty = Nothing
  lookup k (Node arb1 key value arb2)
    | k < key = lookup k arb1
    | k > key = lookup k arb2
    | otherwise = value
  delete k Empty = Empty
  delete k (Node arb1 key value arb2)
    | k < key = Node (delete k arb1) key value arb2
    | k > key = Node arb1 key value (delete k arb2)
    | otherwise = Node arb1 key Nothing arb2
  toList Empty = []
  toList (Node arb1 key Nothing arb2) = toList arb1 ++ toList arb2
  toList (Node arb1 key (Just value) arb2) = toList arb1 ++ [(key, value)] ++ toList arb2
  