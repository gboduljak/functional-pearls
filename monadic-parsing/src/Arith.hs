module Arith (expr, term, addop, mulop, factor, digit, integer, number)
where
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad       (Monad(..), when)
import Data.Char
import Data.List
import Parser (Parser(..), parse, zero)
import Combinators(
  look,
  sat,
  char, string, spaces,
  apply, token, symbol,
  many0, many1,
  sepBy0, sepBy1,
  chainl1,
  failure
  )

expr :: Parser Double
expr = term `chainl1` addop

term :: Parser Double
term = factor `chainl1` mulop

addop :: Parser (Double -> Double -> Double)
addop = add <|> sub
  where add = do { symbol "+"; return (+)}
        sub = do { symbol "-"; return (-)}

mulop :: Parser (Double -> Double -> Double)
mulop = mul <|> div
  where mul = do { symbol "*"; return (*)}
        div = do { symbol "/"; return (/)}

factor :: Parser Double
factor = negativeFactor <|> parensExpr <|> number
  where
    negativeFactor = do { symbol "-"; fmap negate factor }
    parensExpr = do { symbol "("; x <- expr; symbol ")"; return x }

digit :: Parser Int
digit = do {
  d <- sat isDigit;
  return (ord d - ord '0')
}

integer :: Parser Int
integer = do {
  spaces;
  ds <- many digit;
  if not $ null ds then return (asInt ds) else failure
}
  where asInt ds = sum [ d * (10^p) | (d, p) <- zip (reverse ds) [0..] ]

number :: Parser Double
number = withDecimal <|> withoutDecimal
  where
    withoutDecimal = fmap fromIntegral integer
    withDecimal = do {
      wholePart <- withoutDecimal;
      char '.';
      fractionalPart <- fmap asFracPt (many digit);
      return (wholePart + fractionalPart)
    }
    asFracPt ds = sum [ fromIntegral d * (10 ** (-p))| (d, p) <- zip ds [1..] ]