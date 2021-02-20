import Test.QuickCheck
import Test.QuickCheck.Gen

double :: Int -> Int
double = (2 *)

triple :: Int -> Int
triple = (3 *)

penta :: Int -> Int
penta = (5 *)

test x = (double x + triple x) == penta x

testFals x = (double x + triple x) /= penta x

myLookUp :: Int -> [(Int, String)] -> Maybe String
myLookUp _ [] = Nothing
myLookUp k ((key, value) : xs)
  | k == key = Just value
  | otherwise = myLookUp k xs

testLookUp :: Int -> [(Int, String)] -> Bool
testLookUp key env = myLookUp key env == lookup key env

testLookUpCond :: Int -> [(Int, String)] -> Property
testLookUpCond n list = n > 0 && n `div` 5 == 0 ==> testLookUp n list

data ElemIS = I Int | S String
  deriving (Show, Eq)

instance Arbitrary ElemIS where
    --  arbitrary =  oneof [MkGen ( \s i -> I (unGen arbitrary s i)), 
    --                     MkGen ( \s i -> S (unGen arbitrary s i))]
    arbitrary = oneof [I <$> arbitrary, S <$> arbitrary]

myLookUpElem :: Int -> [(Int, ElemIS)] -> Maybe ElemIS
myLookUpElem _ [] = Nothing
myLookUpElem k ((key, value) : xs)
  | k == key = Just value
  | otherwise = myLookUpElem k xs

testLookUpElem :: Int -> [(Int, ElemIS)] -> Bool
testLookUpElem key env = myLookUpElem key env == lookup key env
