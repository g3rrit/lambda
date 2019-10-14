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

newtype Env = Env
  { nameCount :: Integer
  }

emptyEnv :: Env
emptyEnv = Env 0

apply :: Term -> (String, Term) -> Term
apply a s@(n, v) =
  case a of
    Var i   -> if i == n then v else Var i
    Abs i t -> Abs i $ apply t s
    App e t -> App (apply e s) (apply t s)

eval :: Term -> Term
eval = \case
  Var i   -> Var i
  Abs i t -> Abs i (eval t)
  App e t ->
    let (e', t') = (eval e, eval t) in
    case e' of
      Var i -> App e' t'
      Abs i _ -> apply t' (i, t')
      App _ _ -> App e' t'


type IS = State Integer
rename :: Term -> Term
rename t = evalState (rename' (\_ -> Nothing) t) 0
  where rename' sf t = case t of
          Var i   -> case sf i of
                       Just ni -> return $ Var ni
                       Nothing -> return $ Var i
          Abs i t -> case sf i of
                       Just ni -> error $ "multiple abstractions of the variable " ++ i
                       Nothing -> do
                         c <- get
                         modify (+1)
                         t' <- rename' (\s -> if s == i then Just (nname i c) else sf s) t
                         return $ Abs (nname i c) t'
          App e t -> do
            e' <- rename' sf e
            t' <- rename' sf t
            return $ App e' t'
        nname o c = o ++ "__" ++ (show c)

--------------------------------------------------------
-- PARSER
--------------------------------------------------------

type Parser = P.Parsec String Env

parseString :: String -> Either P.ParseError Term
parseString s = P.runParser parseTerm emptyEnv "Main" s

parseTerm :: Parser Term
parseTerm = parseAbs
          <|> parseAbs
          <|> parseApp

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
  l <- parseTerm
  sep
  r <- parseTerm
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
    Right t  -> putStrLn $ "::>" ++ show (eval $ rename t)

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          minp <- getInputLine "|lambda> "
          case minp of
            Nothing  -> outputStrLn "Goodbye!"
            Just inp -> do
              liftIO $ process inp
              loop
