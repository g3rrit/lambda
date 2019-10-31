{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------
-- IMPORTS
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

data Term = Var String
          | Abs String Term
          | App Term Term
          deriving (Show, Eq, Ord)

fvar :: Term -> S.Set String
fvar = \case
  Var i   -> S.singleton i
  Abs i t -> S.delete i $ fvar t
  App e t -> S.union (fvar e) (fvar t)

bvar :: Term -> S.Set String
bvar = \case
  Var i   -> S.empty
  Abs i t -> S.union (S.singleton i) (bvar t)
  App e t -> S.union (bvar e) (bvar t)

alpha :: String -> Int -> Term -> Term
alpha s c = alpha' s False c
  where alpha' s b c = \case
          Var i   -> if i == s && b then Var (i ++ show c) else Var i
          Abs i t -> if i == s then Abs (i ++ show c) (alpha' s True c t) else Abs i (alpha' s b c t)
          App e t -> App (alpha' s b c e) (alpha' s b c t)

type IS = State Int
beta :: Term -> IS Term
beta = \case
  Var i   -> return $ Var i
  Abs i t -> Abs i <$> (beta t)
  App l r -> do
    l' <- beta l
    r' <- beta r
    case l' of
      Var i -> return $ App l' r'
      App ll rr -> return $ App l' r'
      Abs i t -> do
        c <- get
        let df = S.toList $ S.intersection (bvar t) (fvar r')
        let cc = length df + c
        modify (\_ -> cc)
        let t' = foldr (\(s, c') b -> alpha s c' b) t (zip df [c..cc])
        return $ apply t' (i, r')

apply :: Term -> (String, Term) -> Term
apply a s@(n, v) =
  case a of
    Var i   -> if i == n then v else Var i
    Abs i t -> Abs i $ apply t s
    App e t -> App (apply e s) (apply t s)

eval :: Term -> Term
eval t = evalState (beta t) 0

--------------------------------------------------------
-- PARSER
--------------------------------------------------------

type Parser = P.Parsec String ()

oparens :: Parser a -> Parser a
oparens p = (C.char '(' >> sep) *> p <* (sep >> C.char ')')

parseString :: String -> Either P.ParseError Term
parseString s = P.runParser parseTerm () "Main" s

parseTerm :: Parser Term
parseTerm = P.try (oparens parseTerm')
            <|> parseTerm'
  where parseTerm' = parseAbs
                     <|> parseApp
                     <|> parseVar

parseVar :: Parser Term
parseVar = do
  c <- CB.many1 C.alphaNum
  return $ Var c

parseAbs :: Parser Term
parseAbs = do
  C.char '\\'
  sep
  n <- CB.many1 C.alphaNum
  sep
  C.string "->"
  sep
  v <- parseTerm
  return $ Abs n v

parseApp :: Parser Term
parseApp = P.try $ do
  C.char '('
  sep
  l <- parseTerm
  sep
  r <- parseTerm
  sep
  C.char ')'
  return $ App l r

sep :: Parser ()
sep = C.spaces

--------------------------------------------------------
-- APP
--------------------------------------------------------

process :: String -> IO ()
process s =
  case parseString s of
    Left err -> putStrLn $ "Error" ++ show err
    Right t  -> putStrLn $ "::> " ++ show (eval t)

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          minp <- getInputLine "|lambda> "
          case minp of
            Nothing  -> outputStrLn "Goodbye!"
            Just inp -> do
              liftIO $ process inp
              loop
