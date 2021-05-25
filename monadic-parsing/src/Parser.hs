module Parser(Parser(..), parse, zero) 

where
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad       (Monad(..))

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

zero :: Parser a
zero = Parser (const [])

instance Functor Parser where
  fmap g p = Parser (\cs -> case parse p cs of
                          []        -> []
                          [(a,cs')] -> [(g a, cs')])

instance Applicative Parser where
  pure a = Parser (\cs -> [(a, cs)])
  pg <*> pa = Parser (\cs -> case parse pg cs of 
      [] -> []
      [(g, cs')] -> parse (fmap g pa) cs'
    )

instance Alternative Parser where
    empty = zero
    p <|> q = Parser (\cs -> case parse p cs of
      [] -> parse q cs
      [(a, cs')] -> [(a, cs')]
      )

instance Monad Parser where
  return = pure
  p >>= f = Parser (\cs -> concat [
      parse (f a) cs' | (a, cs') <- parse p cs
    ])