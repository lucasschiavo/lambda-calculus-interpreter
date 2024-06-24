module Main where

import AST
import Data.String (fromString)
import Lexer (runAlex)
import Parser (parseMiniML)
import System.IO ()

toLambdaExp :: Prog -> Exp
toLambdaExp (Prog decs exp) = go decs exp
 where
  go [] exp = exp
  go ((Dec name def) : xs) exp = App (Lambda name next) def
   where
    next = go xs exp

isFreeVar :: Name -> Exp -> Bool
isFreeVar name (Lambda varName exp)
  | name == varName = False
  | otherwise = isFreeVar name exp

generateFreeVar :: Name -> Exp -> Name
generateFreeVar name exp
  | isFreeVar name exp = name
  | otherwise = generateFreeVar newName exp
 where
  newName = name ++ "1"

substituteDep :: Dec -> Exp -> Exp
substituteDep (Dec name exp) var@(Var varName)
  | name == varName = exp
  | otherwise = var
substituteDep dec@(Dec name exp) (App exp1 exp2) =
  App (substituteDep dec exp1) (substituteDep dec exp2)
-- for now it does not solve variable capturing
substituteDep dec@(Dec var decExp) lamb@(Lambda varName lambExp)
  | var == varName = lamb
  | otherwise = substituteDep dec lambExp

substitute :: Name -> Exp -> Exp -> Exp
substitute name def var@(Var varName)
  | name == varName = def
  | otherwise = var
substitute name def (App exp1 exp2) =
  App (substitute name def exp1) (substitute name def exp2)
-- does not solve variable capturing
substitute name def lamb@(Lambda varName exp)
  | name == varName = lamb
  | otherwise = Lambda varName $ substitute name def exp

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
    Left err -> return Nothing

main :: IO ()
main = do
  prog <- getProg "teste.lmb"
  case prog of
    Nothing -> putStrLn "error"
    Just prog ->
      do
        let exp = toLambdaExp prog
        runPrint exp
