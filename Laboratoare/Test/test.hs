-- import Data.Char
-- import Test.QuickCheck

-- Scrieți o funcție care are la intrare un șir și care verifică dacă toate vocalele din șir sunt literă mare.
-- 1. rezolvați problema folosind recursivitate (fără descrieri de liste și funcții de nivel înalt)
-- 2. rezolvați problema folosind descrieri de liste
-- 3. rezolvați problema folosind funcții de nivel înalt (map, filter, foldr)
-- 4. Scrieți un test quickcheck care să compare rezolvarea de punctul 2 cu cea de la punctul 1

-- --1
-- f21bmVocaleRec :: [Char] -> Bool
-- f21bmVocaleRec [] = True
-- f21bmVocaleRec (x : xs)
--   | x `elem` "aeiou" = isUpper x && f21bmVocaleRec xs -- pentru fiecare vocala, fac (&&) intre isUpper vocala si rezultatul obtinut in urma prelucrarii cozii sirului
--   | otherwise = f21bmVocaleRec xs -- daca nu este vocala doar prelucrez sirul mai departe
-- -- f21bmVocaleRec "abcdE2" == False
-- -- f21bmVocaleRec "AbcdE2" == True
-- -- f21bmVocaleRec "bcd2" == True

-- --2
-- f21bmVocaleDesc :: [Char] -> Bool
-- f21bmVocaleDesc l = and [isUpper x | x <- l, x `elem` "aeiou"] -- pt fiecare elem din sir, iau in calcul doar vocalele, intorc rezultatul functiei isUpper si pe vectorul obtinut aplic and
-- -- f21bmVocaleDesc "abcdE2" == False
-- -- f21bmVocaleDesc "AbcdE2" == True
-- -- f21bmVocaleDesc "bcd2" == True

-- --3
-- f21bmVocaleInalt :: [Char] -> Bool
-- f21bmVocaleInalt l = foldr (&&) True (map isUpper (filter (`elem` "aeiou") l)) -- filtrez lista, pastrand doar vocalele, verific daca sunt majuscule obtinand o lista de True sau False si apoi fac (&&) pt lista obtinuta
-- -- f21bmVocaleInalt "abcdE2" == False
-- -- f21bmVocaleInalt "AbcdE2" == True
-- -- f21bmVocaleInalt "bcd2" == True

-- --4
-- f21bmVocaleProp :: [Char] -> Bool
-- f21bmVocaleProp x = f21bmVocaleDesc x == f21bmVocaleRec x -- compar rezultatele celor 2 functii pentru un anumit argument
-- -- quickCheck f21bmVocaleProp



-- Scrieți o funcție care transformă o listă de numere întregi după următoarele reguli:

-- * fără 0 înainte, dar cu 0 după fiecare din următoarele numere: 3, 7, 11, 101.
-- * cu 0 înainte, dar fără 0 după numărul 13. 
--  Numărul 0 nu trebuie adăugat dacă există deja.

--  Exemplu:

-- > f [1, 2, 13, 0, 2, 0, 3, 0, 9, 11, 24, 0, 101]
-- [1, 2, 0, 13, 2, 3, 0, 9, 11, 0, 24, 101]

-- -- f21bmRescriere :: [Int] -> [Int]
-- -- f21bmRescriere l = f21bmRescriere' (filter (/= 0) l)

-- -- f21bmRescriere' :: [Int] -> [Int]
-- -- f21bmRescriere' [] = []
-- -- f21bmRescriere' [x]
-- --     | x == 3 || x == 7 || x == 11 || x == 101 = [x, 0]
-- --     | x == 13 = [0, x]
-- --     | otherwise = [x]
-- -- f21bmRescriere' (x:xs) = [f21bmRescriere' a : f21bmRescriere' b | (a, b) <- zip (x:xs) xs]


-- -- f21 (h:t) = f21bmRescriere' 

f21bmRescriere2 :: [Int] -> [Int]
f21bmRescriere2 = foldr aux []
    where       
        aux :: Int -> [Int] -> [Int]
        aux a []
            | a == 3 || a == 7 || a == 11 || a == 101 = [a, 0]
            | a == 13 = [0, a]
            | otherwise = [a]
        aux a (x:xs)
            | (a == 3 || a == 7 || a == 11 || a == 101) && (x == 3 || x == 7 ||x == 11 || x == 101) = [a, x, 0] ++ xs
            | a == 13  && x == 13 = [0, a, x] ++ xs
            | otherwise = aux a [] ++ aux x [] ++ xs
