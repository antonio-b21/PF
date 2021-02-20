import Data.Char

nUMEFUNCTIE :: [Char] -> [Char]
nUMEFUNCTIE l = fUNCTIEAUX2 "" (fUNCTIEAUX "" l)
  where
    fUNCTIEAUX :: [Char] -> [Char] -> [Char]
    fUNCTIEAUX result [] = result
    fUNCTIEAUX result [x] = x : result
    fUNCTIEAUX result (x1 : x2 : xs)
      | x1 == ' ' && x2 == ' ' = fUNCTIEAUX result (x2 : xs)
      | otherwise = fUNCTIEAUX (x1 : result) (x2 : xs)
    fUNCTIEAUX2 :: [Char] -> [Char] -> [Char]
    fUNCTIEAUX2 result [] = result
    fUNCTIEAUX2 result [x] = x : result
    fUNCTIEAUX2 result (x1 : x2 : xs)
      | x1 == ' ' && x2 == '-' = fUNCTIEAUX2 result (x2 : xs)
      | x1 == '-' && x2 == ' ' = fUNCTIEAUX2 (x1 : result) xs
      | otherwise = fUNCTIEAUX2 (x1 : result) (x2 : xs)

nUME2FUNCTIE :: [Char] -> [(Char, Char)]
nUME2FUNCTIE cs = [aUX c | c <- cs]
  where
    aUX :: Char -> (Char, Char)
    aUX c
      | isLower c = (c, '#')
      | isUpper c = (c, '*')
      | isDigit c = (c, '^')
      | otherwise = (c, '_')