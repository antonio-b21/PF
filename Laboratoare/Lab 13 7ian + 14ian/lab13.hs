import Data.Char
import Data.List.Split

-- exp1
prelStr = map toUpper

ioString = do
  strin <- getLine
  putStrLn $ "Intrare\n" ++ strin
  let strout = prelStr strin
  putStrLn $ "Iesire\n" ++ strout

-- exp2
prelNo = sqrt

ioNumber = do
  noin <- readLn :: IO Double
  putStrLn $ "Intrare\n" ++ show noin
  let noout = prelNo noin
  putStrLn $ "Iesire"
  print noout

-- exp3
inoutFile = do
  sin <- readFile "Input.txt"
  putStrLn $ "Intrare\n" ++ sin
  let sout = prelStr sin
  putStrLn $ "Iesire\n" ++ sout
  writeFile "Output.txt" sout

aux1 :: Int -> [String] -> Int -> IO()
aux1 0 lista _ = do
    putStrLn $ concatMap (++ ", ") lista

aux1 m listP ma = do
  nume <- getLine
  varsta <- readLn :: IO Int
  if varsta == ma
    then aux1 (m -1) (nume : listP) ma
    else
      if varsta > ma
        then aux1 (m -1) [nume] varsta
        else aux1 (m -1) listP ma

ex1 = do
  n <- readLn :: IO Int
  aux1 n [] 0


aux2 :: [String] -> (String, Int)
aux2 [a, b] = (a, read b)
ex2 = do
    continut <- readFile "ex2.in"
    let listaLinii = lines continut 
    let persoane = map (aux2 . splitOn ",") listaLinii
    let ma = foldr (\(_, b) c -> max b c ) 0 persoane
    let listaFinala = filter (\(_, b) -> b == ma) persoane
    putStrLn $ concatMap ((++ ", ") . fst) listaFinala
