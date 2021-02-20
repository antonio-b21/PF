data B e
  = e :|: e
  | B e ::: B e

infixr 5 :::

infixr 6 :|:

instance Foldable B where
  foldMap f (e1 :|: e2) = f e1 <> f e2
  foldMap f (b1 ::: b2) = foldMap f b1 <> foldMap f b2

fTest0 = maximum ("Mama" :|: "are" ::: "patru" :|: "mere" ::: "" :|: "si" ::: "doua" :|: "pere") == "si"

class C e where
  cFilter :: Monoid a => (a -> Bool) -> e a -> e a
  toList :: (Monoid a, Eq a) => e a -> [a]

instance C B where
  cFilter prop (e1 :|: e2)
    | prop e1 && prop e2 = e1 :|: e2
    | prop e1 = e1 :|: mempty
    | prop e2 = mempty :|: e2
    | otherwise = mempty :|: mempty
  cFilter prop (b1 ::: b2) = cFilter prop b1 ::: cFilter prop b2

  toList (e1 :|: e2)
    | e1 == mempty && e2 == mempty = []
    | e1 == mempty = [e2]
    | e2 == mempty = [e1]
    | otherwise = e1 : [e2]
  toList (b1 ::: b2) = toList b1 ++ toList b2

cTest0 = toList (cFilter (\x -> length x > 3) ("Mama" :|: "are" ::: "patru" :|: "mere" ::: "" :|: "si" ::: "doua" :|: "pere")) == ["Mama", "patru", "mere", "doua", "pere"]