import Data.List ()

-- L3.1 Încercati sa gasiti valoarea expresiilor de mai jos si
-- verificati raspunsul gasit de voi în interpretor:
{-
[x^2 | x <- [1 .. 10], x `rem` 3 == 2]
[(x, y) | x <- [1 .. 5], y <- [x .. (x+2)]]
[(x, y) | x <- [1 .. 3], let k = x^2, y <- [1 .. k]]
[x | x <- "Facultatea de Matematica si Informatica", elem x ['A' .. 'Z']]
[[x .. y] | x <- [1 .. 5], y <- [1 .. 5], x < y ]

-}

factori :: Int -> [Int]
factori x = [f | f <- [1 .. x], x `mod` f == 0]

prim :: Int -> Bool
prim x = length (factori x) == 2

numerePrime :: Int -> [Int]
numerePrime x = [n | n <- [2 .. x], prim n]

-- L3.2 Testati si sesizati diferenta:
-- [(x,y) | x <- [1..5], y <- [1..3]]
-- zip [1..5] [1..3]

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 (h1 : t1) (h2 : t2) (h3 : t3) = (h1, h2, h3) : myzip3 t1 t2 t3
myzip3 _ _ _ = []

--------------------------------------------------------
----------FUNCTII DE NIVEL INALT -----------------------
--------------------------------------------------------
aplica2 :: (a -> a) -> a -> a
--aplica2 f x = f (f x)
aplica2 f = f . f

--aplica2 f = \x -> f (f x)
-- aplica2 = \f x -> f (f x)

-- L3.3
{-

map (\ x -> 2 * x) [1 .. 10]
map (1 `elem` ) [[2, 3], [1, 2]]
map ( `elem` [2, 3] ) [1, 3, 4, 5]

-}

-- firstEl [ ('a', 3), ('b', 2), ('c', 1)]
firstEl :: [(a, b)] -> [a]
firstEl = map fst

-- sumList [[1, 3],[2, 4, 5], [], [1, 3, 5, 6]]
sumList :: [[Integer]] -> [Integer]
sumList = map sum

-- prel2 [2,4,5,6]
prel2 :: [Integer] -> [Integer]
prel2 = map aux
  where
    aux nr
      | odd nr = nr * 2
      | otherwise = nr `div` 2

-- L3.4

func1 :: Char -> [[Char]] -> [[Char]]
func1 c = filter (elem c)

func2 :: [Int] -> [Int]
func2 = map (^ 2) . filter odd

func3 :: [Int] -> [Int]
func3 lista = map ((^ 2) . snd) (filter (odd . fst) (zip lista [0 ..]))

-- funct3' :: [Int] -> [Int]
funct3' :: Num b => [b] -> [b]
funct3' l =
  let l2 = zip l [0 ..]
   in let l3 = [x | (x, y) <- l2, odd y]
       in map (^ 2) l3

func4 :: [[Char]] -> [[Char]]
func4 = map $ filter (`elem` "aeiouAEIOU")

mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap fun (x : xs) = fun x : mymap fun xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter fun (x : xs)
  | fun x = x : myfilter fun xs
  | otherwise = myfilter fun xs

numerePrimeCiur :: Int -> [Int]
numerePrimeCiur nr = ciur [2 .. nr]

ciur :: [Int] -> [Int]
ciur [] = []
ciur (x : xs) = x : ciur (filter (\y -> y `mod` x /= 0) xs)

ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [_] = True
ordonataNat (x : xs) = and [a < b | (a, b) <- zip (x : xs) xs]

ordonataNatRec :: [Int] -> Bool
ordonataNatRec [] = True
ordonataNatRec [_] = True
ordonataNatRec (x : xs) = x < head xs && ordonataNatRec xs

-- 3
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata [] _ = True
ordonata [_] _ = True
ordonata (x : xs) rel = and [rel a b | (a, b) <- zip (x : xs) xs]

(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(*<*) (a, b) (c, d) = (a < c) && (b < d)

-- 4
compuneList :: (b -> c) -> [a -> b] -> [a -> c]
compuneList f1 = map (f1 .)
-- compuneList f1 funcs = [f1 . f2 | f2 <- funcs]

aplicaList :: a -> [a -> b] -> [b]
aplicaList x = map ($ x)
--aplicaList x funcs = [f x | f <- funcs]

myzip32 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip32 la lb lc = map (\((a, b), c) -> (a, b, c)) (zip (zip la lb) lc)