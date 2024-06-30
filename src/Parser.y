{
{-# LANGUAGE DeriveFoldable #-}
module Parser
  ( parseProgram
  ) where

import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import AST
import qualified Token as T
import qualified Lexer as L
}

%name parseProgram program
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken T.EOF _ }

%token
  identifier { L.RangedToken (T.Identifier _) _ }
  let        { L.RangedToken T.Let _ }
  in         { L.RangedToken T.In _ }
  '='        { L.RangedToken T.Equal _ }
  '('        { L.RangedToken T.LPar _ }
  ')'        { L.RangedToken T.RPar _ }
  ';'        { L.RangedToken T.SColon _ }
  bslash     { L.RangedToken T.BSlash _ }
  '.'        { L.RangedToken T.Dot _ }

%%

name :: { Name }
  : identifier { unTok $1 (\(T.Identifier name) -> unpack name) }

dec :: { Dec }
  : name '=' exp { Dec $1 $3 }

exp :: { Exp }
  : simpleExp { $1 }
  | exp simpleExp { App $1 $2 }

simpleExp :: { Exp }
  : name                { Var $1 }
  | bslash name '.' exp { Lambda $2 $4 }
  | '(' exp ')'         { $2 }

body :: { [Dec] }
  : dec { [$1] }
  | body ';' dec { $3 : $1 }

program :: { Prog }
  : let body in exp     { Prog $2 $4 }
  | let body ';' in exp { Prog $2 $5 }


{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

unTok :: L.RangedToken -> (T.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor tok

info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2
}

