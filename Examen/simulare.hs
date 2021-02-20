data Arbore a
  = Vid
  | Frunza a
  | Nod (Arbore a) a (Arbore a)
  deriving (Eq)

instance Functor Arbore where
  fmap _ Vid = Vid
  fmap f (Frunza v) = Frunza (f v)
  fmap f (Nod aStanga v aDreapta) = Nod (fmap f aStanga) (f v) (fmap f aDreapta)

instance Foldable Arbore where
  foldMap _ Vid = mempty
  foldMap f (Frunza v) = f v
  foldMap f (Nod aStanga v aDreapta) = foldMap f aStanga <> f v <> foldMap f aDreapta

--teste pentru Functor si Foldable si Show?

instance Show a => Show (Arbore a) where
  show = foldr go ""
    where
      go a "" = show a
      go a b = show a ++ ", " ++ b

-- show Vid = ""
-- show (Frunza v) = show v
-- show (Nod Vid v aDreapta) = show v ++ ", " ++ show aDreapta
-- show (Nod aStanga v Vid) = show aStanga ++ ", " ++ show v
-- show (Nod aStanga v aDreapta) = show aStanga ++ ", " ++ show v ++ ", " ++ show aDreapta

p1 :: Arbore Integer
p1 = Frunza 2

p2 :: Arbore Integer
p2 = Nod (Nod Vid 2 (Frunza 3)) 5 Vid

p3 :: Arbore Integer
p3 = Nod (Nod (Frunza 1) 2 (Frunza 5)) 4 Vid

verifica :: (Ord a, Bounded a) => Arbore a -> Bool
verifica arb = foldr go maxBound arb /= minBound
  where
    go a b
      | a < b = a
      | otherwise = minBound

-- verifica (Frunza _) = True
-- verifica (Nod aStanga v aDreapta) =
--   ( eVid aStanga
--       || maxLui aStanga < v && verifica aStanga
--   )
--     && ( eVid aDreapta
--            || v < minLui aDreapta && verifica aDreapta
--        )
--   where
--     eVid :: Arbore a -> Bool
--     eVid Vid = True
--     eVid _ = False

--     maxLui :: Arbore a -> a
--     maxLui (Frunza v) = v
--     maxLui (Nod _ v aDreapta)
--       | eVid aDreapta = v
--       | otherwise = maxLui aDreapta

--     minLui :: Arbore a -> a
--     minLui (Frunza v) = v
--     minLui (Nod aStanga v _)
--       | eVid aDreapta = v
--       | otherwise = minLui aStanga

instance Bounded Integer where
  minBound = -1000
  maxBound = 1000

test_verifica1 :: Bool
test_verifica1 = verifica p1 == True

test_verifica2 :: Bool
test_verifica2 = verifica p2 == True

test_verifica3 :: Bool
test_verifica3 = verifica p3 == False

insereaza :: Ord a => Arbore a -> a -> Arbore a
insereaza Vid val = Frunza val
insereaza (Frunza v) val
  | val < v = Nod (Frunza val) v Vid
  | val > v = Nod Vid v (Frunza val)
  | otherwise = error "deja exista acest element"
--Ce fac daca inserez un element care deja exista?
insereaza (Nod aStanga v aDreapta) val
  | val < v = Nod (insereaza aStanga val) v aDreapta
  | val > v = Nod aStanga v (insereaza aDreapta val)
  | otherwise = error "deja exista acest element"

test_insereaza1 = insereaza p1 5 == Nod Vid 2 (Frunza 5)

test_insereaza2 = insereaza p2 4 == Nod (Nod Vid 2 (Nod Vid 3 (Frunza 4))) 5 Vid

test_insereaza3 = insereaza p2 6 == Nod (Nod Vid 2 (Frunza 3)) 5 (Frunza 6)
