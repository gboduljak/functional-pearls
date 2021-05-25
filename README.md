# Functional Pearls
Implementations of [functional pearls](https://wiki.haskell.org/Research_papers/Functional_pearls) I read.

## Repository structure
Each pearl corresponds to the folder in the root of the repository. Each folder is a Haskell stack project. Therefore,
to run any of pearls, the installation of [Haskell stack](https://docs.haskellstack.org/en/stable/README/) is needed.
Usually, each pearl will be implemented as a program which can be run by executing ```stack run```in the root of folder
corresponding to a pearl.

## Monadic Parsing
[This](http://www.cs.nott.ac.uk/~pszgmh//pearl.pdf) is the first functional pearl I read and implemented. It is a about monadic parsing, also known as parser combinator parsing. I have written [a blog post](https://gabrijel-boduljak.com/functional-pearl-monadic-parsing/)
discussing the implementations in this repository.

Haskell’s extremely powerful type system enables us to write parsers for context-free grammars which looks essentially the same as the grammar productions, which looks quite cool.

To illustrate the main ideas, consider the subset of classic expression grammar in the EBNF form:
```haskell
<expr> ::= <term> "+" <expr>
         | <term> "-" <expr>
         | <term>

<term> ::= <factor> "*" <term>
         | <factor> "\"" <term>
         | <factor>

<factor> ::= "(" <expr> ")"
           | <number>
```

Using parser combinators, we can write an expression evaluator in just a few lines of code.

```haskell
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
```
The definition of Parser monad is [here](https://github.com/gboduljak/functional-pearls/blob/master/monadic-parsing/src/Parser.hs]).
You can find implementations of all combinators in [Combinators.hs](https://github.com/gboduljak/functional-pearls/blob/master/monadic-parsing/src/Combinators.hs).

Using them, the following parsers are defined:
* [Balanced parenthesis parser](https://github.com/gboduljak/functional-pearls/blob/master/monadic-parsing/src/BalancedParenthesis.hs)
* [JSON parser](https://github.com/gboduljak/functional-pearls/blob/master/monadic-parsing/src/JSON.hs)
* [Calculator](https://github.com/gboduljak/functional-pearls/blob/master/monadic-parsing/src/Arith.hs)

To try them, you can run ```stack run``` in the root of ```monadic-parsing``` folder. There is a [small program](https://github.com/gboduljak/functional-pearls/blob/master/monadic-parsing/app/Main.hs) which exposes all of them in 'raw' form :)
