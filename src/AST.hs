module AST where

type Name = String

data Dec
  = Dec Name Exp
  deriving (Show)

data Exp
  = Var Name
  | Lambda Name Exp
  | App Exp Exp
  deriving (Eq)

data Prog
  = Prog [Dec] Exp
  deriving (Show)

instance Show Exp where
  show (Var name) = name
  show (Lambda name exp) = "\\" ++ name ++ "." ++ show exp
  show (App (Var name1) (Var name2)) = name1 ++ " " ++ name2
  show (App (Var name) exp2) = name ++ " (" ++ show exp2 ++ ")"
  show (App exp1 (Var name)) = "(" ++ show exp1 ++ ") " ++ name
  show (App exp1 exp2) = "(" ++ show exp1 ++ ") " ++ "(" ++ show exp2 ++ ")"
