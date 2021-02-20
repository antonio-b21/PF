-- la nevoie decomentati liniile urmatoare:

import Data.Char
import Data.List

---------------------------------------------
-------RECURSIE: FIBONACCI-------------------
---------------------------------------------

fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
  | n < 2 = n
  | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)

fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
  fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)

-- | @fibonacciLiniar@ calculeaza @F(n)@, al @n@-lea element din secvența Fibonacci în timp liniar, folosind funcția auxiliară @fibonacciPereche@ care, dat fiind @n >= 1@ calculează perechea @(F(n-1), F(n))@, evitănd astfel dubla recursie. Completați definiția funcției fibonacciPereche.
--
-- Indicație:  folosiți matching pe perechea calculată de apelul recursiv.
fibonacciLiniar :: Integer -> Integer
fibonacciLiniar 0 = 0
fibonacciLiniar n = snd (fibonacciPereche n)
  where
    fibonacciPereche :: Integer -> (Integer, Integer)
    fibonacciPereche 1 = (0, 1)
    fibonacciPereche n =
      let (precedent, curent) = fibonacciPereche (n - 1)
       in (curent, precedent + curent)

---------------------------------------------
----------RECURSIE PE LISTE -----------------
---------------------------------------------
semiPareRecDestr :: [Int] -> [Int]
semiPareRecDestr l
  | null l = l
  | even h = h `div` 2 : t'
  | otherwise = t'
  where
    h = head l
    t = tail l
    t' = semiPareRecDestr t

semiPareRecEq :: [Int] -> [Int]
semiPareRecEq [] = []
semiPareRecEq (h : t)
  | even h = h `div` 2 : t'
  | otherwise = t'
  where
    t' = semiPareRecEq t

---------------------------------------------
----------DESCRIERI DE LISTE ----------------
---------------------------------------------
semiPareComp :: [Int] -> [Int]
semiPareComp l = [x `div` 2 | x <- l, even x]

-- L2.2
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec lo hi xs
  | null xs = xs
  | (lo <= h) && (h <= hi) = h : t'
  | otherwise = t'
  where
    h = head xs
    t = tail xs
    t' = inIntervalRec lo hi t

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp lo hi xs = [x | x <- xs, lo <= x, x <= hi]

-- L2.3

pozitiveRec :: [Int] -> Int
pozitiveRec l
  | null l = 0
  | h > 0 = 1 + t'
  | otherwise = t'
  where
    h = head l
    t = tail l
    t' = pozitiveRec t

pozitiveComp :: [Int] -> Int
pozitiveComp l = length [x | x <- l, x > 0]

-- L2.4
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec = pozitiiAux 0
  where
    pozitiiAux :: Int -> [Int] -> [Int]
    pozitiiAux _ [] = []
    pozitiiAux n (h : t) =
      let t' = pozitiiAux (n + 1) t
       in if odd h
            then n : t'
            else t'

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [x | (y, x) <- zip l [0 ..], odd y]

-- L2.5

multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (h : t)
  | isDigit h = digitToInt h * t'
  | otherwise = t'
  where
    t' = multDigitsRec t

multDigitsComp :: String -> Int
multDigitsComp sir = product [digitToInt x | x <- sir, isDigit x]

-- L2.6

discountRec :: [Float] -> [Float]
discountRec [] = []
discountRec (h : t)
  | h' < 200 = h' : t'
  | otherwise = t'
  where
    h' = h * 0.75
    t' = discountRec t

discountComp :: [Float] -> [Float]
discountComp list = [y | x <- list, let y = x * 0.75, y < 200]
