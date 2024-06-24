module AST where

type Name = String

data Dec
  = Dec Name Exp
  deriving (Show)

data Exp
  = Var Name
  | Lambda Name Exp
  | App Exp Exp
  deriving (Show, Eq)

data Prog
  = Prog [Dec] Exp
  deriving (Show)
