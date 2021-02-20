--ex1
data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False

goldenCuVierme = Mar "Golden Delicious" True

portocalaSicilia10 = Portocala "Sanguinello" 10

listaFructe =
  [ Mar "Ionatan" False,
    Portocala "Sanguinello" 10,
    Portocala "Valencia" 22,
    Mar "Golden Delicious" True,
    Portocala "Sanguinello" 15,
    Portocala "Moro" 12,
    Portocala "Tarocco" 3,
    Portocala "Moro" 12,
    Portocala "Valencia" 2,
    Mar "Golden Delicious" False,
    Mar "Golden" False,
    Mar "Golden" True
  ]

--ex1 a)
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala soi _) = soi `elem` ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False

test_ePortocalaDeSicilia1 = ePortocalaDeSicilia (Portocala "Moro" 12) == True

test_ePortocalaDeSicilia2 = ePortocalaDeSicilia (Mar "Ionatan" True) == False

--ex1 b)
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia = sum . map (\(Portocala _ nrfelii) -> nrfelii) . filter ePortocalaDeSicilia

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

--ex1 c)
eMar :: Fruct -> Bool
eMar (Mar _ _) = True
eMar _ = False

nrMereViermi :: [Fruct] -> Int
nrMereViermi = length . filter (\(Mar _ viermi) -> viermi) . filter eMar

test_nrMereViermi = nrMereViermi listaFructe == 2

--ex2
newtype Linie = L [Int]
  deriving (Show)

newtype Matrice = M [Linie]

--ex2 a)
verifica :: Matrice -> Int -> Bool
verifica (M ls) n = foldr (\(L l) b -> b && sum l == n) True ls

test_verifica1 = verifica (M [L [1, 2, 3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]) 10 == False

test_verifica2 = verifica (M [L [2, 20, 3], L [4, 21], L [2, 3, 6, 8, 6], L [8, 5, 3, 9]]) 25 == True

--ex2 b)
instance Show Matrice where
  show (M []) = ""
  show (M (l : ls)) = showLinie l ++ "\n" ++ show (M ls)
    where
      showLinie (L []) = ""
      showLinie (L (x : xs)) = show x ++ " " ++ showLinie (L xs)

test_showMatrice = M [L [1, 2, 3], L [4, 5], L [2, 3, 6, 8], L [8, 5, 3]]

--ex2 c)
doarPozN :: Matrice -> Int -> Bool
doarPozN (M ls) n = foldr (\(L l) b -> b && (length l /= n || all (> 0) l)) True ls

test_doarPozN1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True

test_doarPozN2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False