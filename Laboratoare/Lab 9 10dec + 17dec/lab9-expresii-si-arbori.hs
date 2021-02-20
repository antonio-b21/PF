data Expr
  = Const Int -- integer constant
  | Expr :+: Expr -- addition
  | Expr :*: Expr -- multiplication
  deriving (Eq)

data Operation = Add | Mult deriving (Eq, Show)

data Tree
  = Lf Int -- leaf
  | Node Operation Tree Tree -- branch
  deriving (Eq, Show)

--ex1
instance Show Expr where
  show (Const x) = show x
  show (exp1 :+: exp2) = show exp1 ++ " + " ++ show exp2
  show ((exp1 :+: exp2) :*: (exp3 :+: exp4)) = "(" ++ show exp1 ++ " + " ++ show exp2 ++ ") * (" ++ show exp3 ++ " + " ++ show exp4 ++ ")"
  show ((exp1 :+: exp2) :*: exp3) = "(" ++ show exp1 ++ " + " ++ show exp2 ++ ") * " ++ show exp3
  show (exp1 :*: (exp3 :+: exp4)) = show exp1 ++ " * (" ++ show exp3 ++ " + " ++ show exp4 ++ ")"
  show (exp1 :*: exp3) = show exp1 ++ " * " ++ show exp3

--ex2
evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (exp1 :+: exp2) = evalExp exp1 + evalExp exp2
evalExp (exp1 :*: exp2) = evalExp exp1 * evalExp exp2

exp1 = (Const 2 :*: Const 3) :+: (Const 0 :*: Const 5)

exp2 = Const 2 :*: (Const 3 :+: Const 4)

exp3 = Const 4 :+: (Const 3 :*: Const 3)

exp4 = ((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2

test11 = evalExp exp1 == 6

test12 = evalExp exp2 == 14

test13 = evalExp exp3 == 13

test14 = evalExp exp4 == 16

--ex3
evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add arb1 arb2) = evalArb arb1 + evalArb arb2
evalArb (Node Mult arb1 arb2) = evalArb arb1 * evalArb arb2

--ex4
expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (exp1 :+: exp2) = Node Add (expToArb exp1) (expToArb exp2)
expToArb (exp1 :*: exp2) = Node Mult (expToArb exp1) (expToArb exp2)

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0) (Lf 5))

arb2 = Node Mult (Lf 2) (Node Add (Lf 3) (Lf 4))

arb3 = Node Add (Lf 4) (Node Mult (Lf 3) (Lf 3))

arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3) (Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6

test22 = evalArb arb2 == 14

test23 = evalArb arb3 == 13

test24 = evalArb arb4 == 16

--ex5
class MySmallCheck a where
  smallValues :: [a]
  
  smallCheck :: (a -> Bool) -> Bool
  smallCheck prop = and [prop x | x <- smallValues]

instance MySmallCheck Expr where
  smallValues = [exp1, exp2]

checkExp :: Expr -> Bool
checkExp exp = evalExp exp == (evalArb . expToArb) exp
