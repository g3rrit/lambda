{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------
-- Imports
--------------------------------------------------------

import qualified Data.Set as S

import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as CB

import System.Console.Haskeline
import Control.Monad.State

--------------------------------------------------------
-- DATATYPES
--------------------------------------------------------

type Name = (String, Integer)

data Ty = Ty
        | Fn Ty Ty

data Term = Var Name Ty
          | Abs Name Ty Term
          | App Term Term
          | Constant Val

data Val = T | F

o :: Ty -> Integer
o = \case 
  Ty     -> 0
  Fn l r -> max (o l + 1) (o r)

apply :: Name -> Term -> Term -> Term
apply n v = \case 
  Var n' ty -> if n' == n then v else Var n' ty
  Abs n' ty t -> if n' == n then error "variable " ++ show . fst n ++ " bount twice"
                 else Abs n' ty $ apply n v t
  App l r -> App (apply n v l) (apply n v r)
  Constant v -> Constant v

eval :: Term -> Term
eval = \case
  Var n ty   -> Var n ty
  Constant v -> Constant v
  Abs n ty t -> Abs n ty $ eval t
  App l r    -> case (eval l, eval r) of
                  (Abs n ty t, r') -> apply n r' t
                  (l', r' ) -> App l' r'

check :: Term -> Maybe Ty
check = \case 
  Var n ty -> ty

--------------------------------------------------------
-- PARSER
--------------------------------------------------------


