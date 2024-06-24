{
module Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan
  , Range (..)
  , RangedToken (..)
  , scanMany
  ) where

import Control.Monad (when)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Token
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | _) ($alpha | $digit | _ | ' )*

tokens :-

<0> $white+ ;

<0>       "(*" { nestComment `andBegin` comment }
<0>       "*)" { \_ _ -> alexError "Error: unexpected closing comment" }
<comment> "(*" { nestComment }
<comment> "*)" { unnestComment }
<comment> .    ;
<comment> \n   ;

-- Keywords
<0> let     { tok Let }
<0> in      { tok In }

<0> "="     { tok Equal }

-- Parenthesis
<0> "("     { tok LPar }
<0> ")"     { tok RPar }

<0> "\"    { tok BSlash }
<0> ";"    { tok SColon }
<0> "."    { tok Dot }

-- Identifiers
<0> @id     { tokId }

{
data AlexUserState = AlexUserState
  { nestLevel :: Int
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { nestLevel = 0
  }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "Error: unclosed comment"
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Identifier $ BS.take len str
    , rtRange = mkRange inp len
    }

nestComment, unnestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len
unnestComment input len = do
  state <- get
  let level = nestLevel state - 1
  put state{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}
