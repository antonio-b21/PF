import Numeric.Natural
import Test.QuickCheck ()

produsRec :: [Integer] -> Integer
produsRec [] = 1
produsRec (x : xs) = x * produsRec xs

produsFold :: [Integer] -> Integer
produsFold = foldr (*) 1

prop_produs x = produsRec x == produsFold x

andRec :: [Bool] -> Bool
andRec [] = True
andRec (x : xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and x = andRec x == andFold x

concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x : xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat x = concatRec x == concatFold x

rmChar :: Char -> String -> String
rmChar chr = filter (/= chr)

rmCharsRec :: String -> String -> String
rmCharsRec _ "" = ""
rmCharsRec filtru (x : xs)
  | x `elem` filtru = rmCharsRec filtru xs
  | otherwise = x : rmCharsRec filtru xs

test_rmchars :: Bool
test_rmchars = rmCharsRec ['a' .. 'l'] "fotbal" == "ot"

rmCharsFold :: String -> String -> String
rmCharsFold filtru = foldr (auxconcat filtru) ""
  where
    auxconcat filtru ch1 ch2
      | ch1 `elem` filtru = ch2
      | otherwise = ch1 : ch2
