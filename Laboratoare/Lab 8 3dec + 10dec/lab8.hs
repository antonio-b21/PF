import Control.Arrow (Arrow ((&&&)))
import Data.List (nub)
import Data.Maybe (fromJust)

type Nume = String

data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  deriving (Eq, Read)

infixr 2 :|:

infixr 3 :&:

infixr 4 :->:

--ex1
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

--ex2
instance Show Prop where
  show (Var nume) = nume
  show F = "F"
  show T = "T"
  show (Not prop) = "(~" ++ show prop ++ ")"
  show (prop1 :|: prop2) = "(" ++ show prop1 ++ "|" ++ show prop2 ++ ")"
  show (prop1 :&: prop2) = "(" ++ show prop1 ++ "&" ++ show prop2 ++ ")"
  show (prop1 :->: prop2) = "(" ++ show prop1 ++ "->" ++ show prop2 ++ ")"

test_ShowProp :: Bool
test_ShowProp =
  show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

--ex2'
show' :: Prop -> [Char]
show' (Var p) = p
show' T = "T"
show' F = "F"
show' (Not (Var p)) = "~" ++ show' (Var p)
show' (Not p) = "~(" ++ show' p ++ ")"
show' (p1 :|: p2) = show' p1 ++ " | " ++ show' p2
show' ((p1 :|: p2) :&: (p3 :|: p4)) = "(" ++ show' (p1 :|: p2) ++ ") & (" ++ show' (p3 :|: p4) ++ ")"
show' ((p1 :|: p2) :&: p) = "(" ++ show' (p1 :|: p2) ++ ") & " ++ show' p
show' (p :&: (p1 :|: p2)) = show' p ++ " & (" ++ show' (p1 :|: p2) ++ ")"
show' (p :&: q) = show' p ++ " & " ++ show' q

-- Evaluarea expresiilor logice
type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a, b)] -> b
impureLookup a = fromJust . lookup a

--ex3
eval :: Prop -> Env -> Bool
eval (Var prop) env = impureLookup prop env
eval F _ = False
eval T _ = True
eval (Not prop) env = not (eval prop env)
eval (prop1 :|: prop2) env = eval prop1 env || eval prop2 env
eval (prop1 :&: prop2) env = eval prop1 env && eval prop2 env
eval (prop1 :->: prop2) env = not (eval prop1 env) || eval prop2 env

test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

-- Satisfiabilitate

--ex4
variabile :: Prop -> [Nume]
variabile (Var prop) = [prop]
variabile F = []
variabile T = []
variabile (Not prop) = variabile prop
variabile (prop1 :|: prop2) = nub (variabile prop1 ++ variabile prop2)
variabile (prop1 :&: prop2) = nub (variabile prop1 ++ variabile prop2)
variabile (prop1 :->: prop2) = nub (variabile prop1 ++ variabile prop2)

test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

--ex5
gen :: Int -> [[Bool]] --gen 2 == [[True, True], [True, False], [False, True], [True True]]
gen 0 = []
gen 1 = [[False], [True]]
gen n = [False : xs | xs <- gen (n -1)] ++ [True : xs | xs <- gen (n -1)]

envs :: [Nume] -> [Env]
envs xs = map (xs `zip`) (gen (length xs))

test_envs =
  envs ["P", "Q"]
    == [ [ ("P", False),
           ("Q", False)
         ],
         [ ("P", False),
           ("Q", True)
         ],
         [ ("P", True),
           ("Q", False)
         ],
         [ ("P", True),
           ("Q", True)
         ]
       ]

--ex6
satisfiabila :: Prop -> Bool
satisfiabila prop = any (eval prop) (envs (variabile prop))

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True

test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

--ex7
valida :: Prop -> Bool
valida prop = all (eval prop) (envs (variabile prop))

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False

test_valida2 = valida (Not (Var "P") :|: Var "P") == True

--ex8
tabelAdevar :: Prop -> IO ()
tabelAdevar prop =
  putStrLn $
    concat [nume ++ " " | nume <- variabile prop] ++ "| " ++ show' prop ++ "\n"
      ++ concat [concat [afiseaza val ++ " " | (_, val) <- env] ++ "| " ++ afiseaza (eval prop env) ++ "\n" | env <- envs (variabile prop)]
  where
    afiseaza True = "T"
    afiseaza False = "F"

-- Implicatie si echivalenta

--ex10
echivalenta :: Prop -> Prop -> Bool
echivalenta p1 p2 = all (uncurry (==) . (eval p1 &&& eval p2)) (envs (variabile (p1 :|: p2)))

-- echivalenta2 p1 p2 = and (map (\(x, y) -> x == y) (map (eval p1) (envs (variabile (p1 :|: p2))) `zip` map (eval p2) (envs (variabile (p1 :|: p2)))))

test_echivalenta1 = (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q"))) == True

test_echivalenta2 = (Var "P") `echivalenta` (Var "Q") == False

test_echivalenta3 = (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q")) == True
