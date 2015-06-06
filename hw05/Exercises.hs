import ExprT
import Parser

--01. write an evaluation function for ExprT
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y

--02. evaluate arithmetic expressions given as a string
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul


--03. create a typeclass to represent ExprT
class Expr a where
  add :: Expr a => a -> a -> a
  mul :: Expr a => a -> a -> a
  lit :: Expr a => a
