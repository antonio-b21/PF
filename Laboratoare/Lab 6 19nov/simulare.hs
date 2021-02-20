import Data.Char

-- exercitiul 1

f :: Char -> Bool
f chr
    | not . isLetter $ chr = error "Nu este litera"
    | chr <= 'M' = True
    | chr >= 'a' && chr <= 'm' = True
    | otherwise = False

g :: String -> Bool
g str = length [x | x <- str, isLetter x, f x] > length [x | x <- str, isLetter x, not . f $ x]

h :: String -> Bool
h str = (>0) $ count 0 str
    where
        count cnt [] = cnt
        count cnt (x:xs)
            | isLetter x && f x = count (cnt+1) xs
            | isLetter x && not (f x) = count (cnt-1) xs
            | otherwise = count cnt xs

-- exercitiul 2

c :: [Int] -> [Int]
c [] = []
c (x:xs) = [a | (a, b) <- zip (x:xs) xs, a == b]

d :: [Int] -> [Int]
d [] = []
d [_] = []
d (x:xs)
    | x == head xs = x : d xs
    | otherwise = d xs

prop_cd :: [Int] -> Bool
prop_cd lst = c lst == d lst

-- prop_cd [3,1,1,3,3,5]
-- prop_cd [4,1,1,1,4,4]
-- prop_cd [2,2,2,2,2]