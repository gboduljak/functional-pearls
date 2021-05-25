{-# LANGUAGE LambdaCase #-}
module Combinators(look, sat, char, string, spaces, apply, token, symbol, many0, many1, sepBy0, sepBy1, takeUntil, chainl1, failure, Combinators.empty)
where
import Parser (Parser(..), parse, zero)
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad       (Monad(..))
import Data.Char (isSpace)
import Data.List(foldl, null)

look :: Parser (Maybe Char)
look = Parser (\case
      [] -> [(Nothing, [])]
      (c:cs') -> [(Just c, c:cs')]
    )

item :: Parser Char
item = Parser (\case
    "" -> []
    (c:cs) -> [(c,cs)]
  )

takeUntil :: Char -> Parser [Char]
takeUntil stop = consumeRest "" stop
  where
  consumeRest acc stop = do {
    l <- look;
    if l == Just stop then return [] else do {
      c <- item;
      cs <- consumeRest (acc ++ [c]) stop;
      return (c:cs)
    }
  }

failure :: Parser a
failure = Parser (const [])

empty :: Parser Bool
empty = Parser (\case
    [] -> [(True, [])]
    cs -> [(False, cs)] 
  ) 

sat :: (Char -> Bool) -> Parser Char
sat p = do { c <- item; if p c then return c else zero }

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string = foldr (\c acc -> do {
            pc <- char c;
            pr <- acc;
            return (pc:pr)
          }
        ) (return "")

spaces :: Parser String
spaces = many0 (sat isSpace)

apply :: Parser a -> String -> [(a, String)]
apply p = parse (do { spaces; p })

token :: Parser a -> Parser a
token p = do { spaces; a <- p; spaces; return a }

symbol :: String -> Parser String
symbol symb = token (string symb)

many0 :: Parser a -> Parser [a]
many0 p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do {
  a <- p;
  as <- many0 p;
  return (a : as)
}

sepBy0 :: Parser a -> Parser b -> Parser [a]
p `sepBy0` sep = p `sepBy1` sep <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do {
  a <- p;
  as <- many0 (do { sep; p });
  return (a:as)
}

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { x <- p; rest x }
  where rest x = do {
    f <- op;
    y <- p;
    rest (f x y)
  } <|> return x