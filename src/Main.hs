module Main (main) where

import AST
import Data.String (fromString)
import Interpreter
import Lexer (runAlex)
import Parser (parseProgram)
import System.IO ()

getProg :: String -> IO (Maybe Prog)
getProg path = do
  file <- readFile path
  case runAlex (fromString file) parseProgram of
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
