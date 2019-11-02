{-# LANGUAGE LambdaCase #-}

-- Example Input:
-- ((((\c : (t -> (t -> t)) . \l : t . \r : t . ((c l) r)) (\x : t . \y : t . x)) F) T)
-- if c then l else r 

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
  deriving Eq

instance Show Ty where
  show = \case 
    Ty     -> "t"
    Fn l r -> show l ++ " -> " ++ show r

data Term = Var Name Ty
          | Abs Name Ty Term
          | App Term Term
          | Const Val

instance Show Term where
  show = \case 
    Var n t    -> fst n
    Abs n ty t -> "\\ " ++ (fst n) ++ " : " ++ show ty ++ " . " ++ show t
    App l r -> show l ++ " " ++ show r
    Const v -> show v

data Val = T | F
  deriving Show

o :: Ty -> Integer
o = \case 
  Ty     -> 0
  Fn l r -> max (o l + 1) (o r)

apply :: Name -> Term -> Term -> Term
apply n v = \case 
  Var n' ty -> if n' == n then v else Var n' ty
  Abs n' ty t -> if n' == n 
                 then error $ "variable " ++ (show $ fst n) ++ " bount twice"
                 else Abs n' ty $ apply n v t
  App l r -> App (apply n v l) (apply n v r)
  Const v -> Const v

eval :: Term -> Term
eval = \case
  Var n ty   -> Var n ty
  Const v    -> Const v
  Abs n ty t -> Abs n ty $ eval t
  App l r    -> case (eval l, eval r) of
                  (Abs n ty t, r') -> eval $ apply n r' t
                  (l', r' ) -> App l' r'

check :: Term -> Maybe Ty
check = \case 
  Var n ty -> return ty
  Const v -> return Ty
  Abs n ty t -> Fn ty <$> check t
  App l r -> do
    tl <- check l
    tr <- check r
    case tl of
      Fn tfl tfr -> 
        if tfl == tr 
        then return tfr
        else Nothing
      _ -> Nothing

--------------------------------------------------------
-- PARSER
--------------------------------------------------------

type Parser = P.Parsec String Env 

oparens :: Parser a -> Parser a
oparens p = (C.char '(' >> sep) *> p <* (sep >> C.char ')')

parseString :: String -> Either P.ParseError Term
parseString s = P.runParser parseTerm env0 "Main" s

parseTerm :: Parser Term
parseTerm = P.try (oparens parseTerm')
            <|> parseTerm'
  where parseTerm' = parseAbs
                     <|> parseApp
                     <|> parseConst
                     <|> parseVar


type Env = (Integer, String -> (Integer, Ty))

env0 :: Env
env0 = (0, \s -> error $ "variable " ++ show s ++ " not defined")

sep :: Parser ()
sep = C.spaces

parseVar :: Parser Term
parseVar = do
  n <- CB.many1 C.alphaNum
  (_, e) <- P.getState
  let (c, t) = e n
  return $ Var (n, c) t

parseAbs :: Parser Term
parseAbs = do
  C.char '\\'
  sep
  n <- CB.many1 C.alphaNum
  sep
  C.char ':'
  sep
  ty <- parseTy

  (c, e) <- P.getState
  let c' = c + 1
  let e' = \s -> if s == n then (c', ty) else e s
  P.putState (c', e')

  sep
  C.char '.'
  sep
  v <- parseTerm
  return $ Abs (n, c') ty v

parseApp :: Parser Term
parseApp = do
  C.char '('
  sep
  l <- parseTerm
  sep
  r <- parseTerm
  sep
  C.char ')'
  return $ App l r

parseConst :: Parser Term
parseConst = Const <$> ((C.char 'T' >> return T) <|> (C.char 'F' >> return F))

parseTy :: Parser Ty
parseTy = ((C.char 't') >> return Ty) <|> do
  C.char '('
  sep
  l <- parseTy
  sep
  C.string "->"
  sep
  r <- parseTy
  sep 
  C.char ')'
  return $ Fn l r

--------------------------------------------------------
-- APP
--------------------------------------------------------

process :: String -> IO ()
process s =
  case parseString s of
    Left err -> putStrLn $ "Error" ++ show err
    Right t  -> putStrLn $ "::> " ++ case check t of 
                                       Just ty -> (show $ eval t) ++ " : " ++ show ty
                                       Nothing -> "type error"

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          minp <- getInputLine "|lambda> "
          case minp of
            Nothing  -> outputStrLn "Goodbye!"
            Just inp -> do
              liftIO $ process inp
              loop




