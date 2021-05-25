{-# LANGUAGE BlockArguments #-}
module Main where
import BalancedParenthesis (Parens(..), balancedParens, areParensBalanced)
import Combinators(apply)
import JSON(json)
import Arith(expr)
import Data.Char(digitToInt)

main :: IO ()
main = do {
    putStrLn "Hi. Choose a powerful parser :) and then enter a single line to parse.";
    putStrLn "For balanced parenthesis enter 0.";
    putStrLn "For JSON enter 1.";
    putStrLn "For calculator enter 2.";
    putStrLn "To exit enter 3.";
    choice <- fmap (digitToInt . head) getLine;
    case choice of
        0 -> balancedParensIo
        1 -> jsonIo
        2 -> calculatorIo
        3 -> return ()
        _ -> main
    }
    where balancedParensIo :: IO ()
          balancedParensIo = do {
            putStrLn "Enter a string of parens...";
            areBalanced <- fmap areParensBalanced getLine;
            if areBalanced then 
              putStrLn "They are balanced.";
            else 
              putStrLn "They are not balanced.";
            main
          }
          jsonIo :: IO ()
          jsonIo = do {
            putStrLn "Enter a string of json...";
            result <- fmap (apply json) getLine;
            case result of 
              [(parsedJson, "")] -> do {
                print parsedJson;
                main
              }
              _ -> do {
                putStrLn "invalid json :(";
                main
              }
          }
          calculatorIo :: IO ()
          calculatorIo = do {
            putStrLn "Enter an arithmetic expression to evaluate...";
            result <- fmap (apply expr) getLine;
            case result of 
              [(value, "")] -> do {
                print value;
                main
              }
              [(value, unparsed)] -> do {
                putStrLn "The expression is not entirely valid.";
                putStrLn ("The result so far is : " ++ show value ++ ".");
                putStrLn ("Confusing part is: " ++ show unparsed);
                main
              }
              _ -> do {
                putStrLn "The expression is not valid.";
                main
              }
          }
