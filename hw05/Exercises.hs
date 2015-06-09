{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import ExprT
import Parser
import qualified Data.Map as M
import qualified StackVM as VM

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
  add :: a -> a -> a
  mul :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  add = Add
  mul = Mul
  lit = Lit

reify :: ExprT -> ExprT
reify = id

instance Expr Bool where
  add = (||)
  mul = (&&)
  lit = (<= 0)

instance Expr Integer where
  add = (+)
  mul = (*)
  lit = id

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  add (MinMax x) (MinMax y) = MinMax $ min x y
  mul (MinMax x) (MinMax y) = MinMax $ max x y
  lit = MinMax

instance Expr Mod7 where
  add (Mod7 x) (Mod7 y) = Mod7 $ (add x y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (mul x y) `mod` 7
  lit = Mod7 . (`mod` 7)

--05.
instance Expr VM.Program where
  add x y = x ++ y ++ [VM.Add]
  mul x y = x ++ y ++ [VM.Mul]
  lit x   = [VM.PushI x]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

--06.
class HasVars a where
  var :: String -> a

data VarExprT = LitV Integer
              | AddV VarExprT VarExprT
              | MulV VarExprT VarExprT
              | Var String
  deriving (Eq, Show)

instance Expr VarExprT where
  add = AddV
  mul = MulV
  lit = LitV

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer)


-- testing ex04
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMinMax :: Maybe MinMax
testMinMax = testExp

testMod7 :: Maybe Mod7
testMod7 = testExp
