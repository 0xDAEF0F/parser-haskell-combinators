module MyLib (someFunc) where

import Control.Applicative (Alternative (..))
import Data.List (nub)

data Error i e 
  = EndOfInput
  | Unexpected i
  | CustomError e
  | Empty 
  deriving (Eq, Show)

newtype Parser i e a = Parser { runParser :: [i] -> Either [Error i e] (a, [i]) }

instance Functor (Parser i e) where 
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')

instance Monad (Parser i e) where 
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    x : xs
      | predicate x -> Right (x, xs)
      | otherwise -> Left [Unexpected x]

char :: Eq i => i -> Parser i e i
char i = satisfy (== i)

string :: Eq i => [i] -> Parser i e [i]
string [] = return []
string (x:xs) = do
  y <- char x
  ys <- string xs
  return (y:ys)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
