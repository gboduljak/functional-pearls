module BalancedParenthesis(Parens(..), balancedParens, areParensBalanced)
where
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad       (Monad(..), when)
import Data.Char ( isDigit, ord )
import Data.List
import Parser (Parser(..), parse, zero)
import Combinators(symbol,empty,many0, apply)

data Parens = Nested [Parens] | Empty;

instance Show Parens where
    show Empty = ""
    show (Nested parens) = show parens

balancedParens :: Parser [Parens]
balancedParens = many0 (do {
    symbol "(";
    parens <- balancedParens;
    symbol ")";
    return (Nested parens)
}) <|> do { Combinators.empty; return [Empty] };

areParensBalanced :: String -> Bool
areParensBalanced ps
   | null consumed = False
   | null ((snd.head) consumed) = True
   | otherwise = False
   where consumed = apply balancedParens ps