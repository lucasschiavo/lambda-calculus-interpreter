module Token where

import Data.ByteString.Lazy.Char8 (ByteString)

data Token
  = Identifier ByteString
  | Number Integer
  | Let
  | In
  | Equal
  | LPar
  | RPar
  | BSlash
  | SColon
  | Dot
  | EOF
  deriving (Eq, Show)
