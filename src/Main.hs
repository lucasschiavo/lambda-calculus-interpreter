module Main (main) where

import AST (Prog)
import Data.String (fromString)
import Interpreter (run, runPrint, toLambdaExp)
import Lexer (runAlex)
import Parser (parseProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

getProg :: String -> IO (Maybe Prog)
getProg path = do
  file <- readFile path
  case runAlex (fromString file) parseProgram of
    Right ast -> return $ Just ast
    Left err -> do
      print err
      return Nothing

-- TODO: make verbose work
parse :: [String] -> IO ()
parse ["-h"] = usage >> exitSuccess
parse ["-v"] = putStrLn "verbose version" >> exitSuccess
parse [] = usage >> exitSuccess
parse (file : s) = readAndRun file

usage = do
  putStrLn "Interpreter for lambda calculus"
  putStrLn "Usage: stack run -- <file.lam>"
  putStrLn "With flags: stack run -- -v <file.lam>"

main = getArgs >>= parse

readAndRun :: String -> IO ()
readAndRun file = do
  prog <- getProg file
  case prog of
    Nothing -> putStrLn "error" >> exitFailure
    Just prog -> print . run . toLambdaExp $ prog
