module Parser where

import Control.Applicative
import Data.Char
import Data.List (foldl')

{-# ANN module "HLint: ignore Use mappend" #-}

newtype Parser a = P (String -> [(a, String)])

char :: Char -> Parser Char
char c = matches (== c)

matches :: (Char -> Bool) -> Parser Char
matches p = P $ \s ->
  case s of
    (x:xs) | p x -> [(x, xs)]
    _            -> []

instance Functor Parser where
  fmap f (P p) = P $ \s -> [ (f x, s') | (x, s') <- p s ]

instance Applicative Parser where
  pure x           = P $ \s -> [(x, s)]
  (P pf) <*> (P q) = P $ \s -> [ (f x, s'') | (f, s') <- pf s, (x, s'') <- q s' ]

instance Alternative Parser where
  empty           = P $ const []
  (P p) <|> (P q) = P $ \s -> p s ++ q s

instance Monad Parser where
  return = pure
  --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
  --qs    :: Parser (Parser b) ~ P ( String -> [(Parser b, Strng)]
  p >>= f = P $ (\s -> case (f <$> p) of
                            (P qs) -> concat [ qs' s' | (P qs', s') <- qs s ] )


times :: Int -> Parser a -> Parser [a]
times 0 _ = pure []
times n p = (:) <$> p <*> (times (n-1) p)

--parseNInts :: Parser [Int]
--parseNInts = (decimal <* whitespace) >>= (\n -> times n (decimal <* whitespace))

token :: String -> Parser String
token ""     = pure ""
token (x:xs) = (:) <$> char x <*> token xs

digit :: Parser Int
digit = digitToInt <$> matches isDigit

decimal :: Parser Int
decimal = foldl' (\n x -> 10 * n + fromIntegral x) 0 <$> some digit

string :: Parser String
string = (char '"' *> many (matches (/= '"')) <* char '"')

whitespace :: Parser String
whitespace = many (matches isSpace)

endParsing :: Parser ()
endParsing = P $ const [((), "")]

runParser :: Parser a -> String -> Maybe a
runParser (P p) s =
  case (dropWhile (\(x,s) -> not (null s)) $ p s) of
    ((x,""):_) -> Just x
    _          -> Nothing

-- Examples for the monad instance
{-
parseNInts :: Parser [Int]
parseNInts = do
{
  n   <-         (decimal <* whitespace);
  res <- times n (decimal <* whitespace);
  return res;
}

parseNTests :: Parser [[Int]]
parseNTests = do
{
  numOfTests <- decimal';
  tests <- times numOfTests (do
  {
    n    <-         decimal';
    ints <- times n decimal';
    return ints;
  });

  return tests;
}
  where decimal' = decimal <* whitespace
-}
