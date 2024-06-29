module Main (main) where

import AST
import Data.String (fromString)
import Debug.Trace (trace)
import Lexer (runAlex)
import Parser (parseMiniML)
import System.IO ()

toLambdaExp :: Prog -> Exp
toLambdaExp (Prog decs exp) = go (reverse decs) exp
 where
  go [] exp = exp
  go ((Dec name def) : xs) exp = App (Lambda name next) def
   where
    next = go xs exp

alphaConvert :: Name -> Name -> Exp -> Exp
alphaConvert name oldName lamb@(Lambda _ exp) =
  Lambda name $ substitute oldName (Var name) exp

isFreeVar :: Name -> Exp -> Bool
isFreeVar name (Var varName) = name == varName
isFreeVar name (Lambda varName exp) = name /= varName && isFreeVar name exp
isFreeVar name (App exp1 exp2) = isFreeVar name exp1 || isFreeVar name exp2

generateNewName :: Name -> Exp -> Name
generateNewName name exp
  | isFreeVar name exp = generateNewName newName exp
  | otherwise = name
 where
  newName = name ++ "'"

-- Lambda "x" (Lambda "y" (App (Var "x") (App (Var "y") (Var "z"))))
substitute :: Name -> Exp -> Exp -> Exp
substitute name def var@(Var varName)
  | name == varName = def
  | otherwise = var
substitute name def (App exp1 exp2) =
  App (substitute name def exp1) (substitute name def exp2)
substitute name def lamb@(Lambda varName exp)
  | name == varName = lamb
  | not $ isFreeVar varName def = Lambda varName $ substitute name def exp
  | otherwise = Lambda newVar (substitute name def (substitute varName (Var newVar) exp))
 where
  newVar = generateNewName varName (App def exp)

redex :: Exp -> Exp -> Exp
redex lamb@(Lambda name exp) right =
  substitute name right exp
redex _ right = right

step :: Exp -> Exp
step var@(Var{}) = var
step (Lambda name exp) = Lambda name (step exp)
step (App lamb@(Lambda{}) right) = redex lamb right
step (App left right) = App (step left) right

run :: Exp -> Exp
run exp
  | exp == next = next
  | otherwise = run next
 where
  next = step exp

runPrint :: Exp -> IO ()
runPrint exp = do
  let next = step exp
  print next
  if next == exp
    then return ()
    else runPrint next

getProg :: String -> IO (Maybe Prog)
getProg path = do
  file <- readFile path
  case runAlex (fromString file) parseMiniML of
    Right ast -> return $ Just ast
    Left err -> do
      print err
      return Nothing

main :: IO ()
main = do
  prog <- getProg "teste.lmb"
  case prog of
    Nothing -> putStrLn "error"
    Just prog ->
      do
        let exp = toLambdaExp prog
        print exp
        runPrint exp
